"
I am a meta-object that can be put on any AST node to count execution

To install a watchpoint in a node: 
	ExecutionCounter installOn: aNode
	
Class CounterIconStyler then renders an icon in the editor, mouse over it to see the current counter value.
"
Class {
	#name : #ExecutionCounter,
	#superclass : #Object,
	#instVars : [
		'node',
		'link',
		'count'
	],
	#classVars : [
		'Counters'
	],
	#category : #'Reflectivity-Breakpoints'
}

{ #category : #accessing }
ExecutionCounter class >> allCounters [
	^ Counters ifNil: [ Counters := WeakKeyDictionary new ]
]

{ #category : #all }
ExecutionCounter class >> debugWorldMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Reset Counters')
		order: 0;
		parent: #Debugging;
		help: 'Reset the counters on the executions counters.';
		action: [ ExecutionCounter resetAll ]
]

{ #category : #'system annoucements' }
ExecutionCounter class >> handleClassRemoved: anAnnouncement [
	self allCounters copy do: [ :breakpoint |
		breakpoint link methods
			detect: [ :m | m methodClass = anAnnouncement classRemoved ]
			ifFound: [ self allCounters remove: breakpoint ] ]
]

{ #category : #'system annoucements' }
ExecutionCounter class >> handleMethodModified: anAnnouncement [
	self removeFromMethod: anAnnouncement oldMethod
]

{ #category : #'system annoucements' }
ExecutionCounter class >> handleMethodRemoved: anAnnouncement [
	self removeFromMethod: anAnnouncement method
]

{ #category : #'class initialization' }
ExecutionCounter class >> initialize [
	self registerInsterestToSystemAnnouncement 
]

{ #category : #'instance creation' }
ExecutionCounter class >> installOn: aRBProgramNode [
	^self new
		node: aRBProgramNode;
		install.
]

{ #category : #'class initialization' }
ExecutionCounter class >> registerInsterestToSystemAnnouncement [
	<systemEventRegistration>

	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak when: MethodRemoved send: #handleMethodRemoved: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodModified send: #handleMethodModified: to: self.
	SystemAnnouncer uniqueInstance weak when: ClassRemoved send: #handleClassRemoved: to: self
]

{ #category : #cleanup }
ExecutionCounter class >> removeAll [
	self allCounters values do: [ :aWatchpoint| aWatchpoint uninstall].
	Counters := nil.
]

{ #category : #cleanup }
ExecutionCounter class >> removeFrom: aNode [

	(self allCounters at: aNode ifAbsent: [ ^self ]) uninstall
]

{ #category : #cleanup }
ExecutionCounter class >> removeFromMethod: aMethod [ 
	self allCounters copy do: [ :counter |
		counter link methods
			detect: [ :m | m == aMethod ]
			ifFound: [ counter uninstall] ]
]

{ #category : #cleanup }
ExecutionCounter class >> resetAll [
	self allCounters values do: [ :counter | counter reset ].
	Counters := nil
]

{ #category : #accessing }
ExecutionCounter >> count [
	^ count
]

{ #category : #counter }
ExecutionCounter >> increase [
	count := count + 1
]

{ #category : #initialization }
ExecutionCounter >> initialize [
	super initialize.
	count := 0.
]

{ #category : #'add/remove' }
ExecutionCounter >> install [

	link := MetaLink new 
				metaObject: self;
				selector: #increase;
				optionAnnounce: true.
	node link: link.
	self class allCounters at: node put: self.
]

{ #category : #accessing }
ExecutionCounter >> link [
	^ link
]

{ #category : #accessing }
ExecutionCounter >> node [
	^ node
]

{ #category : #accessing }
ExecutionCounter >> node: anObject [
	node := anObject
]

{ #category : #initialization }
ExecutionCounter >> reset [
	count := 0
]

{ #category : #'add/remove' }
ExecutionCounter >> uninstall [
	node hasExecutionCounter ifFalse: [ ^self ].
	link uninstall.
	self class allCounters removeKey: node.
]
