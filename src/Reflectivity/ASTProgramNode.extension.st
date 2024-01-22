Extension { #name : 'ASTProgramNode' }

{ #category : '*Reflectivity' }
ASTProgramNode >> addBreakpoint: aBreakpoint [
	self breakpoints add: aBreakpoint
]

{ #category : '*Reflectivity' }
ASTProgramNode >> afterHooks [
	^self propertyAt: #afterHooks ifAbsent: #()
]

{ #category : '*Reflectivity' }
ASTProgramNode >> afterLinks [
	^ self links select: [ :each | each control == #after ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> allAfterAreWeak [
	self hasMetalink ifFalse: [^false].
	self isMessage ifFalse:  [^false]. "only sends are weak"
	^self afterLinks allSatisfy: [ :link | link hasOption: #optionWeakAfter ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> availableReifications [
	^RFReification subclasses
		select: [ :e | e entities includesAny: self class withAllSuperclasses ]
		thenCollect: #key
]

{ #category : '*Reflectivity' }
ASTProgramNode >> beforeHooks [
	^self propertyAt: #beforeHooks ifAbsentPut: OrderedCollection new
]

{ #category : '*Reflectivity' }
ASTProgramNode >> beforeLinks [
	^ self links select: [ :each | each control == #before ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> breakpoints [
	^ self propertyAt: #breakpoints ifAbsentPut: [ Set new ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> clearReflectivityAnnotations [
	self removeProperty: #preambles ifAbsent: [ ].
	self removeProperty: #beforeHooks ifAbsent: [ ].
	self removeProperty: #afterHooks ifAbsent: [ ].
	self removeProperty: #insteadHooks ifAbsent: [ ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> counter [

	self hasExecutionCounter ifFalse:[^nil].
	^(self beforeLinks detect: [ :link | link metaObject class = ExecutionCounter]) metaObject
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasBeenExecuted [
	^self propertyAt: #tagExecuted ifAbsent: false
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasBreakpoint [
	^ (self hasProperty: #breakpoints) and: [ self breakpoints notEmpty ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasExecutionCounter [

	self hasMetalinkBefore ifFalse: [ ^false ].
	^self beforeLinks anySatisfy: [ :link | link metaObject class = ExecutionCounter ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasLinks [
	self
		deprecated: 'use #hasMetaLinks'
		transformWith:  '`@receiver hasLinks' -> '`@receiver hasMetaLinks'.
	^self hasMetaLinks
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasMetaLinks [
	^ self links isNotNil and: [ self links notEmpty ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasMetalink [
	^self hasProperty: #links
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasMetalink: aMetaLink [
	^self links includes: aMetaLink
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasMetalinkAfter [
	self hasMetalink ifFalse: [^false].
	^self afterLinks isNotEmpty
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasMetalinkBefore [
	self hasMetalink ifFalse: [^false].
	^self beforeLinks isNotEmpty
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasMetalinkInstead [
		self hasMetalink ifFalse: [^false].
		^self insteadLinks isNotEmpty
]

{ #category : '*Reflectivity' }
ASTProgramNode >> hasWatch [

	self hasMetalinkAfter ifFalse: [ ^false ].
	^self afterLinks anySatisfy: [ :link | link metaObject class = Watch ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> insteadHooks [
	^self propertyAt: #insteadHooks ifAbsent: #()
]

{ #category : '*Reflectivity' }
ASTProgramNode >> insteadLinks [
	^ self links select: [ :each | each control == #instead ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> invalidate [

	self methodNode compiledMethod invalidate
]

{ #category : '*Reflectivity' }
ASTProgramNode >> isEquivalentTo: aNode [
	self flag: 'should be tested, does not work on methods after links because of the wrappping now in place'.
	^ aNode methodNode selector = self methodNode selector
		and: [ aNode = self and: [ aNode start = self start and: [ aNode stop = self stop ] ] ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> link: aMetaLink [

	(aMetaLink checkForCompatibilityWith: self) ifFalse: [
		self error:
			'link requests reification that can not be provided by this node' ].
	(self propertyAt: #links ifAbsentPut: [ OrderedCollection new ])
		add: aMetaLink.
	aMetaLink installOn: self.
	self clearReflectivityAnnotations.
	(self methodNode methodClass>>self methodNode selector) installLink: aMetaLink.
	aMetaLink linkInstaller
		propagateLinkAddition: aMetaLink
		forNode: self.
	aMetaLink announceInstall: self
]

{ #category : '*Reflectivity' }
ASTProgramNode >> link: aMetaLink forObject: anObject [
	aMetaLink linkToNode: self forObject: anObject
]

{ #category : '*Reflectivity' }
ASTProgramNode >> linkIfAbsent: metalink [
	"Per-object check only.
	Links and nodes are stored in IdentitySet so there is already a guarantee that there will be no duplicates. However putting a link already existing for a given node will retrigger method compilation.
	This check is used by MetaLinkInstaller only to avoid multiple and recursive reinstallation of links on nodes."
	(self hasMetaLinks and: [ self links includes: metalink ])
		ifTrue: [ ^ self ].
	self link: metalink
]

{ #category : '*Reflectivity' }
ASTProgramNode >> links [
	^self propertyAt: #links ifAbsent: #()
]

{ #category : '*Reflectivity' }
ASTProgramNode >> nodesWithLinks [
	^ self allChildren select: [ :e | e links isNotEmpty ]
]

{ #category : '*Reflectivity' }
ASTProgramNode >> postambles [
	^self propertyAt: #postambles ifAbsent: #()
]

{ #category : '*Reflectivity' }
ASTProgramNode >> preambles [
	^self propertyAt: #preambles ifAbsent: #()
]

{ #category : '*Reflectivity' }
ASTProgramNode >> removeBreakpoint: aBreakpoint [
	self breakpoints remove: aBreakpoint ifAbsent: []
]

{ #category : '*Reflectivity' }
ASTProgramNode >> removeLink: aMetaLink [

	self hasMetalink ifFalse: [ ^ self ].
	self links remove: aMetaLink ifAbsent: [  ].
	self links ifEmpty: [ self removeProperty: #links ].
	self clearReflectivityAnnotations.
	(self methodNode methodClass methodDict includesKey:
		 self methodNode selector) ifFalse: [ ^ self ].
	self methodNode compiledMethod removeLink: aMetaLink.
	aMetaLink linkInstaller propagateLinkRemoval: aMetaLink forNode: self.
	aMetaLink announceRemove: self
]

{ #category : '*Reflectivity' }
ASTProgramNode >> removeLink: aMetaLink forObject: anObject [
	aMetaLink unlinkFromNode: self forObject: anObject
]

{ #category : '*Reflectivity' }
ASTProgramNode >> tagExecuted [
	^self propertyAt: #tagExecuted put: true
]

{ #category : '*Reflectivity' }
ASTProgramNode >> variableValueInContext: aContext [
	^nil
]

{ #category : '*Reflectivity' }
ASTProgramNode >> watch [

	self hasWatch ifFalse:[^nil].
	^(self afterLinks detect: [ :link | link metaObject class = Watch ]) metaObject
]
