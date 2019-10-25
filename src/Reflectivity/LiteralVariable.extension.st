Extension { #name : #LiteralVariable }

{ #category : #'*Reflectivity' }
LiteralVariable >> afterHooks [
	^self propertyAt: #afterHooks ifAbsent: #()
]

{ #category : #'*Reflectivity' }
LiteralVariable >> afterLinks [
	^ self links select: [ :each | each control == #after ]
]

{ #category : #'*Reflectivity' }
LiteralVariable >> assignmentNodes [
	^self astNodes 
		select: [ :each | each isWrite ] 
		thenCollect: [ :each | each parent ]
]

{ #category : #'*Reflectivity' }
LiteralVariable >> astNodes [
	"by default we search the usingMethods for all AST nodes"
	^self usingMethods flatCollect: [ :method | 
		method variableNodes select: [: each| each binding variable == self ]]
]

{ #category : #'*Reflectivity' }
LiteralVariable >> availableReifications [
	^RFReification subclasses 
		select: [ :e | e entities includesAny: self class withAllSuperclasses ]
		thenCollect: #key
]

{ #category : #'*Reflectivity' }
LiteralVariable >> beforeHooks [
	^self propertyAt: #beforeHooks ifAbsentPut: OrderedCollection new
]

{ #category : #'*Reflectivity' }
LiteralVariable >> beforeLinks [
	^ self links select: [ :each | each control == #before ]
]

{ #category : #'*Reflectivity' }
LiteralVariable >> clearReflectivityAnnotations [
	self removeProperty: #preambles ifAbsent: [ ].
	self removeProperty: #beforeHooks ifAbsent: [ ].
	self removeProperty: #afterHooks ifAbsent: [ ].
	self removeProperty: #insteadHooks ifAbsent: [ ].
]

{ #category : #'*Reflectivity' }
LiteralVariable >> hasMetalink [
	^ self hasProperty: #links
]

{ #category : #'*Reflectivity' }
LiteralVariable >> hasMetalinkAfter [
	self hasMetalink
		ifFalse: [ ^ false ].
	^ self afterLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
LiteralVariable >> hasMetalinkBefore [
	self hasMetalink ifFalse: [ ^ false ].
	^ self beforeLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
LiteralVariable >> hasMetalinkInstead [
	self hasMetalink
		ifFalse: [ ^ false ].
	^ self insteadLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
LiteralVariable >> insteadHooks [
	^self propertyAt: #insteadHooks ifAbsent: #()
]

{ #category : #'*Reflectivity' }
LiteralVariable >> insteadLinks [
	^ self links select: [ :each | each control == #instead ]
]

{ #category : #'*Reflectivity' }
LiteralVariable >> isCascade [
	self flag: #hack.
	^false
]

{ #category : #'*Reflectivity' }
LiteralVariable >> link: aMetaLink [
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
LiteralVariable >> links [
	^self propertyAt: #links ifAbsent: #()
]

{ #category : #'*Reflectivity' }
LiteralVariable >> methodNode [
	self flag: #hack.
	^nil
]

{ #category : #'*Reflectivity' }
LiteralVariable >> postambles [
	^ self propertyAt: #postambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
LiteralVariable >> preambles [
	^ self propertyAt: #preambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
LiteralVariable >> readNodes [
	^ self astNodes select: [ :each | each isRead ]
]

{ #category : #'*Reflectivity' }
LiteralVariable >> removeLink: aMetaLink [
	self usingMethods do: [:method | method removeLink: aMetaLink].
	self links remove: aMetaLink.
	self links ifEmpty: [ self removeProperty: #links ].
	self clearReflectivityAnnotations.
]
