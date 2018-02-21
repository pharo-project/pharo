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
RBProgramNode >> hasBeenExecuted [
	^self propertyAt: #tagExecuted ifAbsent: false
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasMetalink [
	^self hasProperty: #links
]

{ #category : #'*Reflectivity' }
RBProgramNode >> hasMetalink: aMetaLink [
	^self links isNotNil and: [(self links includes: aMetaLink)]
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
RBProgramNode >> link: aMetaLink [
	(aMetaLink checkForCompatibilityWith: self) ifFalse: [ self error: 'link requests reification that can not be provided by this node' ].
	(self propertyAt: #links ifAbsentPut: [ OrderedCollection new ]) add: aMetaLink.
	aMetaLink installOn: self.
	self clearReflectivityAnnotations.
	self methodNode method installLink: aMetaLink. 
]

{ #category : #'*Reflectivity' }
RBProgramNode >> links [
	^self propertyAt: #links ifAbsent: nil
]

{ #category : #'*Reflectivity' }
RBProgramNode >> nodesWithLinks [
	^ self allChildren select: [ :e | e links isNotNil ]
]

{ #category : #'*Reflectivity' }
RBProgramNode >> preambles [
	^self propertyAt: #preambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
RBProgramNode >> removeLink: aMetaLink [

	self hasMetalink ifFalse: [ ^ self ].
	
	self links remove: aMetaLink ifAbsent: [].
	self links ifEmpty: [ self removeProperty: #links ].
	self clearReflectivityAnnotations.
	self methodNode method removeLink: aMetaLink. 
]

{ #category : #'*Reflectivity' }
RBProgramNode >> tagExecuted [
	^self propertyAt: #tagExecuted put: true 
]
