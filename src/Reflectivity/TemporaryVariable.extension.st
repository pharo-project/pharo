Extension { #name : #TemporaryVariable }

{ #category : #'*Reflectivity' }
TemporaryVariable >> afterLinks [
	^ self links select: [ :each | each control == #after ]
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> assignmentNodes [
	^ (self astNodes select: [ :ast | ast isWrite ])
		collect: [ :node | node parent ]
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> astNodes [
	self flag: #pharoFixMe. "will return all temps with the same name... as we do not allow shadowing this is ok but shoudl be fixed later"
	^self method variableNodes select: [ :each | (each name = self name)]
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> beforeHooks [
	^self propertyAt: #beforeHooks ifAbsentPut: OrderedCollection new
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> beforeLinks [
	^ self links select: [ :each | each control == #before ]
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> clearReflectivityAnnotations [
	self removeProperty: #preambles ifAbsent: [ ].
	self removeProperty: #beforeHooks ifAbsent: [ ].
	self removeProperty: #afterHooks ifAbsent: [ ].
	self removeProperty: #insteadHooks ifAbsent: [ ].
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> hasMetalink [
	^ self hasProperty: #links
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> hasMetalinkAfter [
	self hasMetalink
		ifFalse: [ ^ false ].
	^ self afterLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> hasMetalinkBefore [
	self hasMetalink ifFalse: [ ^ false ].
	^ self beforeLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> hasMetalinkInstead [
	self hasMetalink
		ifFalse: [ ^ false ].
	^ self insteadLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> insteadLinks [
	^ self links select: [ :each | each control == #instead ]
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> link: aMetaLink [
	(aMetaLink checkForCompatibilityWith: self) ifFalse: [ self error: 'link requests reification that can not be provided by this node' ].
	
	(self propertyAt: #links ifAbsentPut: [ OrderedCollection new ]) add: aMetaLink.
	aMetaLink installOnVariable: self.
	method 
		createTwin;
		invalidate.
	method installLink: aMetaLink
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> links [
	^self propertyAt: #links ifAbsent: #()
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> postambles [
	^ self propertyAt: #postambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> preambles [
	^ self propertyAt: #preambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> readNodes [
	^ self astNodes select: [ :ast | ast isRead ]
]

{ #category : #'*Reflectivity' }
TemporaryVariable >> removeLink: aMetaLink [
	self usingMethods do: [:mth | mth removeLink: aMetaLink].
	self links remove: aMetaLink.
	self links ifEmpty: [ self removeProperty: #links ].
	self clearReflectivityAnnotations.
]
