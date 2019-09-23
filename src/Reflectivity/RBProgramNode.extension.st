Extension { #name : #RBProgramNode }

{ #category : #'*Reflectivity' }
RBProgramNode >> afterHooks [
	^self propertyAt: #afterHooks ifAbsent: #()
]

{ #category : #'*Reflectivity' }
RBProgramNode >> afterLinks [
	^ self links select: [ :each | each control == #after ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> allAfterAreWeak [
	self hasMetalink ifFalse: [^false].
	self isMessage ifFalse:  [^false]. "only sends are weak"
	^self afterLinks allSatisfy: [ :link | link hasOption: #optionWeakAfter ].
]

{ #category : #'*Reflectivity' }
RBProgramNode >> availableReifications [
	^RFReification subclasses 
		select: [ :e | e entities includesAny: self class withAllSuperclasses ]
		thenCollect: #key

]

{ #category : #'*Reflectivity' }
RBProgramNode >> beforeHooks [
	^self propertyAt: #beforeHooks ifAbsentPut: OrderedCollection new
]

{ #category : #'*Reflectivity' }
RBProgramNode >> beforeLinks [
	^ self links select: [ :each | each control == #before ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> clearReflectivityAnnotations [
	self removeProperty: #preambles ifAbsent: [ ].
	self removeProperty: #beforeHooks ifAbsent: [ ].
	self removeProperty: #afterHooks ifAbsent: [ ].
	self removeProperty: #insteadHooks ifAbsent: [ ].
]

{ #category : #'*Reflectivity' }
RBProgramNode >> counter [
	
	self hasExecutionCounter ifFalse:[^nil].
	^(self beforeLinks detect: [ :link | link metaObject class = ExecutionCounter]) metaObject
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasBeenExecuted [
	^self propertyAt: #tagExecuted ifAbsent: false
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasBreakpoint [
	
	self hasMetalinkBefore ifFalse: [ ^false ].
	^self beforeLinks anySatisfy: [ :link | link metaObject = Break ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasExecutionCounter [
	
	self hasMetalinkBefore ifFalse: [ ^false ].
	^self beforeLinks anySatisfy: [ :link | link metaObject class = ExecutionCounter ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasLinks [
	^ self links notNil and: [ self links notEmpty ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasMetalink [
	^self hasProperty: #links
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasMetalink: aMetaLink [
	^self links includes: aMetaLink
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasMetalinkAfter [
	self hasMetalink ifFalse: [^false].
	^self afterLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasMetalinkBefore [
	self hasMetalink ifFalse: [^false].
	^self beforeLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasMetalinkInstead [
		self hasMetalink ifFalse: [^false].
		^self insteadLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasWatchpoint [
	
	self hasMetalinkAfter ifFalse: [ ^false ].
	^self afterLinks anySatisfy: [ :link | link metaObject class = Watchpoint ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> insteadHooks [
	^self propertyAt: #insteadHooks ifAbsent: #()
]

{ #category : #'*Reflectivity' }
RBProgramNode >> insteadLinks [
	^ self links select: [ :each | each control == #instead ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> invalidate [
	self methodNode method invalidate. 
]

{ #category : #'*Reflectivity' }
RBProgramNode >> isEquivalentTo: aNode [
	self flag: 'should be tested, does not work on methods after links because of the wrappping now in place'.
	^ aNode methodNode selector = self methodNode selector
		and: [ aNode = self and: [ aNode start = self start and: [ aNode stop = self stop ] ] ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> link: aMetaLink [
	(aMetaLink checkForCompatibilityWith: self)
		ifFalse: [ self error: 'link requests reification that can not be provided by this node' ].
	(self propertyAt: #links ifAbsentPut: [ OrderedCollection new ]) add: aMetaLink.
	aMetaLink installOn: self.
	self clearReflectivityAnnotations.
	self methodNode method installLink: aMetaLink.
	SystemAnnouncer uniqueInstance announce: (MetalinkChanged linkAdded: aMetaLink toNode: self)
]

{ #category : #'*Reflectivity' }
RBProgramNode >> link: aMetaLink forObject: anObject [
	aMetaLink linkToNode: self forObject: anObject
]

{ #category : #'*Reflectivity' }
RBProgramNode >> linkIfAbsent: metalink [
	"Per-object check only.
	Links and nodes are stored in IdentitySet so there is already a guarantee that there will be no duplicates. However putting a link already existing for a given node will retrigger method compilation.
	This check is used by MetaLinkInstaller only to avoid multiple and recursive reinstallation of links on nodes." 
	(self hasLinks and: [ self links includes: metalink ])
		ifTrue: [ ^ self ].
	self link: metalink
]

{ #category : #'*Reflectivity' }
RBProgramNode >> links [
	^self propertyAt: #links ifAbsent: #()
]

{ #category : #'*Reflectivity' }
RBProgramNode >> nodesWithLinks [
	^ self allChildren select: [ :e | e links isNotEmpty ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> postambles [
	^self propertyAt: #postambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
RBProgramNode >> preambles [
	^self propertyAt: #preambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
RBProgramNode >> removeLink: aMetaLink [
	self hasMetalink
		ifFalse: [ ^ self ].
	self links remove: aMetaLink ifAbsent: [  ].
	self links ifEmpty: [ self removeProperty: #links ].
	self clearReflectivityAnnotations.
	(self methodNode methodClass methodDict includesKey: self methodNode selector)
		ifFalse: [ ^ self ].
	self methodNode method removeLink: aMetaLink.
	SystemAnnouncer uniqueInstance announce: (MetalinkChanged linkRemoved: aMetaLink fromNode: self)
]

{ #category : #'*Reflectivity' }
RBProgramNode >> removeLink: aMetaLink forObject: anObject [
	aMetaLink unlinkFromNode: self forObject: anObject
]

{ #category : #'*Reflectivity' }
RBProgramNode >> tagExecuted [
	^self propertyAt: #tagExecuted put: true 
]

{ #category : #'*Reflectivity' }
RBProgramNode >> watchpoint [
	
	self hasWatchpoint ifFalse:[^nil].
	^(self afterLinks detect: [ :link | link metaObject class = Watchpoint ]) metaObject
]
