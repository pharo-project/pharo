Extension { #name : #Slot }

{ #category : #'*Reflectivity' }
Slot >> afterHooks [
	^self propertyAt: #afterHooks ifAbsent: #()
]

{ #category : #'*Reflectivity' }
Slot >> afterLinks [
	^ self links select: [ :each | each control == #after ]
]

{ #category : #'*Reflectivity' }
Slot >> assignmentNodes [
	^self astNodes 
		select: [ :each | each isWrite ] 
		thenCollect: [ :each | each parent ]
]

{ #category : #'*Reflectivity' }
Slot >> astNodes [
	^self owningClass variableNodes select: [ :each | each binding variable == self ]
]

{ #category : #'*Reflectivity' }
Slot >> availableReifications [
	^RFReification subclasses 
		select: [ :e | e entities includesAny: self class withAllSuperclasses ]
		thenCollect: #key
]

{ #category : #'*Reflectivity' }
Slot >> beforeHooks [
	^self propertyAt: #beforeHooks ifAbsentPut: OrderedCollection new
]

{ #category : #'*Reflectivity' }
Slot >> beforeLinks [
	^ self links select: [ :each | each control == #before ]
]

{ #category : #'*Reflectivity' }
Slot >> clearReflectivityAnnotations [
	self removeProperty: #preambles ifAbsent: [ ].
	self removeProperty: #beforeHooks ifAbsent: [ ].
	self removeProperty: #afterHooks ifAbsent: [ ].
	self removeProperty: #insteadHooks ifAbsent: [ ].
]

{ #category : #'*Reflectivity' }
Slot >> hasMetalink [
	^ self hasProperty: #links
]

{ #category : #'*Reflectivity' }
Slot >> hasMetalinkAfter [
	self hasMetalink
		ifFalse: [ ^ false ].
	^ self afterLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
Slot >> hasMetalinkBefore [
	self hasMetalink ifFalse: [ ^ false ].
	^ self beforeLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
Slot >> hasMetalinkInstead [
	self hasMetalink
		ifFalse: [ ^ false ].
	^ self insteadLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
Slot >> insteadLinks [
	^ self links select: [ :each | each control == #instead ]
]

{ #category : #'*Reflectivity' }
Slot >> link: aMetaLink [
	| methods |
	
	(aMetaLink checkForCompatibilityWith: self) ifFalse: [ self error: 'link requests reification that can not be provided by this node' ].
	
	methods := self usingMethods.
	(self propertyAt: #links ifAbsentPut: [ OrderedCollection new ]) add: aMetaLink.
	aMetaLink installOnVariable: self.
	methods
		do: [ :method |  
			method
				createTwin;
				invalidate.
			method installLink: aMetaLink ]
]

{ #category : #'*Reflectivity' }
Slot >> links [
	^self propertyAt: #links ifAbsent: #()
]

{ #category : #'*Reflectivity' }
Slot >> postambles [
	^ self propertyAt: #postambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
Slot >> preambles [
	^ self propertyAt: #preambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
Slot >> readNodes [
	^ self astNodes select: [ :each | each isRead ]
]

{ #category : #'*Reflectivity' }
Slot >> removeLink: aMetaLink [
	self links remove: aMetaLink.
	self usingMethods do: [:method | method removeLink: aMetaLink].
	self links ifEmpty: [ self removeProperty: #links ].
	self clearReflectivityAnnotations.
]
