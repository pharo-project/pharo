"
I cache all message nodes that are halts in the system. 
There is already a breakpoint cache in the Breakpoint class.

The cache is built when I am initialized,  then I register to the method changes system announcement to update the cache each time a method is added, modified or removed.

I associate the selector of methods in which I found halts to a set holding the AST nodes of those halts.

For now it is a simple cache that does not require as much functionality than AbstractCache provides, so it does not subclass it.
However, it might be interesting if I evolve later, or if I need safe concurrent access (which AbstractCache provides).

By default, the cache does not lookup halts in all the system's methods. This can be enabled in a settings under tools>debugging>halt cache. The cache must be refreshed to effectively perform the lookup (StHaltCache refresh).
"
Class {
	#name : #StHaltCache,
	#superclass : #Object,
	#traits : 'TObservable',
	#classTraits : 'TObservable classTrait',
	#instVars : [
		'#isActive => ObservableSlot',
		'#methodsWithHalts',
		'#scanAllSystem'
	],
	#classVars : [
		'#DefaultCache'
	],
	#category : #'NewTools-Debugger-Breakpoints-Tools-Model'
}

{ #category : #'accessing - cache' }
StHaltCache class >> defaultCache [
	^ DefaultCache ifNil: [ DefaultCache := self new ]
]

{ #category : #settings }
StHaltCache class >> haltCacheSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #haltcache)
		label: 'Halt Cache';
		parent: #debugging;
		description: 'Halt cache settings';
		with: [ 
			(aBuilder setting: #scanAllSystem)
				label:
					'Perform a system-wide lookup to find all halts and breakpoints';
				target: self defaultCache;
				default: false;
				description:
					'If true, then the halt cache will check for halts in all methods from the system. 
This can be slow for large systems (with lots of methods).' ];
		with: [ 
			(aBuilder setting: #isActive)
				label: 'Subscribe to method changes';
				target: self defaultCache;
				default: false;
				description:
					'Activates or deactivate system subscriptions to method changes. 
If deactivated, the list of halts will not be updated in the debugging tools browsers.' ]
]

{ #category : #'accessing - cache' }
StHaltCache class >> refresh [
	<script>
	self defaultCache refreshCache
]

{ #category : #'accessing - cache' }
StHaltCache class >> reset [
	<script>
	DefaultCache
		ifNotNil: [ DefaultCache removeSubscriptions.
			DefaultCache := nil ]
]

{ #category : #cache }
StHaltCache >> buildCache [
	self scanAllSystem ifFalse:[^self].
	SystemNavigation new allHalts
		do: [ :compiledMethod | self buildCacheForMethod: compiledMethod ]
]

{ #category : #cache }
StHaltCache >> buildCacheForMethod: aCompiledMethod [
	"- extract all halt nodes from the method's ast
	- extract all nodes with installed breakpoints from the method's ast
	- cache all extracted nodes into the methodsWithHalts dictionary"

	| ast haltNodes |
	(self shouldConsiderMethod: aCompiledMethod)
		ifFalse: [ ^ self ].
	ast := aCompiledMethod ast.
	haltNodes := ast sendNodes
		select: [ :node | self shouldConsiderHaltNode: node ].
	haltNodes
		ifEmpty: [ self uncacheMethod: aCompiledMethod printString.
			^ self ].
	self setNodeCache: haltNodes forSelector: aCompiledMethod printString.
	self cacheChanged
]

{ #category : #notifying }
StHaltCache >> cacheChanged [
	SystemAnnouncer uniqueInstance announce: StHaltCacheChanged
]

{ #category : #cache }
StHaltCache >> cacheNode: aNode forSelector: aMethodSelector [
	(methodsWithHalts at: aMethodSelector ifAbsentPut: IdentitySet new)
		add: aNode
]

{ #category : #initialization }
StHaltCache >> initialize [
	methodsWithHalts := Dictionary new.
	self buildCache.
	self class initializeSlots: self.
	self property: #isActive whenChangedDo: [ self updateSubscriptions ]
]

{ #category : #accessing }
StHaltCache >> isActive [
	^ isActive ifNil: [ isActive := false ]
]

{ #category : #accessing }
StHaltCache >> isActive: aBoolean [
	isActive := aBoolean
]

{ #category : #notifying }
StHaltCache >> methodAdded: methodAddedAnnouncement [
	self buildCacheForMethod: methodAddedAnnouncement methodAdded
]

{ #category : #notifying }
StHaltCache >> methodChanged: methodCModifiedAnnouncement [
	self buildCacheForMethod: methodCModifiedAnnouncement newMethod
]

{ #category : #notifying }
StHaltCache >> methodRemoved: methodRemovedAnnouncement [
	self uncacheMethod: methodRemovedAnnouncement methodRemoved printString
]

{ #category : #accessing }
StHaltCache >> methodsWithHalts [
	^ methodsWithHalts
]

{ #category : #cache }
StHaltCache >> nodeCacheForSelector: aMethodSelector [
	^ methodsWithHalts at: aMethodSelector ifAbsent: #()
]

{ #category : #cache }
StHaltCache >> refreshCache [
	self buildCache
]

{ #category : #'system subscription' }
StHaltCache >> registerSubscriptions [
	SystemAnnouncer uniqueInstance weak when: MethodModified send: #methodChanged: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodRemoved send: #methodRemoved: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodAdded send: #methodAdded: to: self
]

{ #category : #'system subscription' }
StHaltCache >> removeSubscriptions [
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #accessing }
StHaltCache >> scanAllSystem [
	^scanAllSystem ifNil:[scanAllSystem := false] 
]

{ #category : #accessing }
StHaltCache >> scanAllSystem: aBoolean [
	scanAllSystem := aBoolean
]

{ #category : #cache }
StHaltCache >> setNodeCache: haltAndBreakpointNodes forSelector: aSelector [
	methodsWithHalts at: aSelector put: haltAndBreakpointNodes
]

{ #category : #testing }
StHaltCache >> shouldConsiderHaltNode: aNode [
	| selector shouldRejectNode |
	selector := aNode selector.
	shouldRejectNode := ('*haltOnRead*' match: selector)
		or: [ ('*haltOnWrite*' match: selector)
				or: [ '*haltOnAccess*' match: selector ] ].
	^ shouldRejectNode not and: [ 'halt*' match: selector ]
]

{ #category : #testing }
StHaltCache >> shouldConsiderMethod: aCompiledMethod [

	"We do not consider test or debugging methods"

	^ #( #haltOrBreakpointForTesting #haltOrBreakpointForDevelopment
	     #debuggerCompleteToSender ) noneSatisfy: [ :pragmaName | 
		  aCompiledMethod hasPragmaNamed: pragmaName ]
]

{ #category : #cache }
StHaltCache >> uncacheMethod: aMethodSelector [
	"Methods not in the cache are irrelevant, so we don't care if they are absent from the dictionary"

	(methodsWithHalts keys includes: aMethodSelector)
		ifFalse: [ ^ self ].
	methodsWithHalts removeKey: aMethodSelector.
	self cacheChanged
]

{ #category : #'system subscription' }
StHaltCache >> updateSubscriptions [
	self isActive ifTrue: [ ^ self registerSubscriptions ].
	^ self removeSubscriptions
]
