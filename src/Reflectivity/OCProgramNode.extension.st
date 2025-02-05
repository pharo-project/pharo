Extension { #name : 'OCProgramNode' }

{ #category : '*Reflectivity' }
OCProgramNode >> addBreakpoint: aBreakpoint [
	self breakpoints add: aBreakpoint
]

{ #category : '*Reflectivity' }
OCProgramNode >> afterHooks [
	^self propertyAt: #afterHooks ifAbsent: #()
]

{ #category : '*Reflectivity' }
OCProgramNode >> afterLinks [
	^ self links select: [ :each | each control == #after ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> allAfterAreWeak [
	self hasMetalink ifFalse: [^false].
	self isMessage ifFalse:  [^false]. "only sends are weak"
	^self afterLinks allSatisfy: [ :link | link hasOption: #optionWeakAfter ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> availableReifications [
	^RFReification subclasses
		select: [ :e | e entities includesAny: self class withAllSuperclasses ]
		thenCollect: #key
]

{ #category : '*Reflectivity' }
OCProgramNode >> beforeHooks [
	^self propertyAt: #beforeHooks ifAbsentPut: OrderedCollection new
]

{ #category : '*Reflectivity' }
OCProgramNode >> beforeLinks [
	^ self links select: [ :each | each control == #before ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> breakpoints [
	^ self propertyAt: #breakpoints ifAbsentPut: [ Set new ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> clearReflectivityAnnotations [
	self removeProperty: #preambles ifAbsent: [ ].
	self removeProperty: #beforeHooks ifAbsent: [ ].
	self removeProperty: #afterHooks ifAbsent: [ ].
	self removeProperty: #insteadHooks ifAbsent: [ ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> counter [

	self hasExecutionCounter ifFalse:[^nil].
	^(self beforeLinks detect: [ :link | link metaObject class = ExecutionCounter]) metaObject
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasBeenExecuted [
	^self propertyAt: #tagExecuted ifAbsent: false
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasBreakpoint [
	^ (self hasProperty: #breakpoints) and: [ self breakpoints notEmpty ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasExecutionCounter [

	self hasMetalinkBefore ifFalse: [ ^false ].
	^self beforeLinks anySatisfy: [ :link | link metaObject class = ExecutionCounter ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasLinks [
	self
		deprecated: 'use #hasMetaLinks'
		transformWith:  '`@receiver hasLinks' -> '`@receiver hasMetaLinks'.
	^self hasMetaLinks
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasMetaLinks [
	^ self links isNotNil and: [ self links notEmpty ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasMetalink [
	^self hasProperty: #links
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasMetalink: aMetaLink [
	^self links includes: aMetaLink
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasMetalinkAfter [
	self hasMetalink ifFalse: [^false].
	^self afterLinks isNotEmpty
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasMetalinkBefore [
	self hasMetalink ifFalse: [^false].
	^self beforeLinks isNotEmpty
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasMetalinkInstead [
		self hasMetalink ifFalse: [^false].
		^self insteadLinks isNotEmpty
]

{ #category : '*Reflectivity' }
OCProgramNode >> hasWatch [

	self hasMetalinkAfter ifFalse: [ ^false ].
	^self afterLinks anySatisfy: [ :link | link metaObject class = Watch ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> insteadHooks [
	^self propertyAt: #insteadHooks ifAbsent: #()
]

{ #category : '*Reflectivity' }
OCProgramNode >> insteadLinks [
	^ self links select: [ :each | each control == #instead ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> invalidate [

	self methodNode compiledMethod invalidate
]

{ #category : '*Reflectivity' }
OCProgramNode >> isEquivalentTo: aNode [
	self flag: 'should be tested, does not work on methods after links because of the wrappping now in place'.
	^ aNode methodNode selector = self methodNode selector
		and: [ aNode = self and: [ aNode start = self start and: [ aNode stop = self stop ] ] ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> link: aMetaLink [

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
OCProgramNode >> link: aMetaLink forObject: anObject [
	aMetaLink linkToNode: self forObject: anObject
]

{ #category : '*Reflectivity' }
OCProgramNode >> linkIfAbsent: metalink [
	"Per-object check only.
	Links and nodes are stored in IdentitySet so there is already a guarantee that there will be no duplicates. However putting a link already existing for a given node will retrigger method compilation.
	This check is used by MetaLinkInstaller only to avoid multiple and recursive reinstallation of links on nodes."
	(self hasMetaLinks and: [ self links includes: metalink ])
		ifTrue: [ ^ self ].
	self link: metalink
]

{ #category : '*Reflectivity' }
OCProgramNode >> links [
	^self propertyAt: #links ifAbsent: #()
]

{ #category : '*Reflectivity' }
OCProgramNode >> nodesWithLinks [
	^ self allChildren select: [ :e | e links isNotEmpty ]
]

{ #category : '*Reflectivity' }
OCProgramNode >> postambles [
	^self propertyAt: #postambles ifAbsent: #()
]

{ #category : '*Reflectivity' }
OCProgramNode >> preambles [
	^self propertyAt: #preambles ifAbsent: #()
]

{ #category : '*Reflectivity' }
OCProgramNode >> removeBreakpoint: aBreakpoint [
	self breakpoints remove: aBreakpoint ifAbsent: []
]

{ #category : '*Reflectivity' }
OCProgramNode >> removeLink: aMetaLink [

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
OCProgramNode >> removeLink: aMetaLink forObject: anObject [
	aMetaLink unlinkFromNode: self forObject: anObject
]

{ #category : '*Reflectivity' }
OCProgramNode >> tagExecuted [
	^self propertyAt: #tagExecuted put: true
]

{ #category : '*Reflectivity' }
OCProgramNode >> variableValueInContext: aContext [
	^nil
]

{ #category : '*Reflectivity' }
OCProgramNode >> watch [

	self hasWatch ifFalse:[^nil].
	^(self afterLinks detect: [ :link | link metaObject class = Watch ]) metaObject
]
