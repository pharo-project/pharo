Extension { #name : #Variable }

{ #category : #'*Reflectivity' }
Variable >> accessingNodes [
	"return all reads and all assignments. This is different to #astNodes, which returns 
	variable nodes in case of assignment, not the assignment itself"
	^ self readNodes, self assignmentNodes
]

{ #category : #'*Reflectivity' }
Variable >> accessingNodesFor: option [
	option = #write
		ifTrue: [ ^ self assignmentNodes ].
	option = #read
		ifTrue: [ ^ self  readNodes ].
	"else we return all accessing nodes"
	^ self accessingNodes
]

{ #category : #'*Reflectivity' }
Variable >> afterHooks [
	^self propertyAt: #afterHooks ifAbsent: #()
]

{ #category : #'*Reflectivity' }
Variable >> afterLinks [
	^ self links select: [ :each | each control == #after ]
]

{ #category : #'*Reflectivity' }
Variable >> assignmentNodes [
	^self astNodes 
		select: [ :each | each isWrite ] 
		thenCollect: [ :each | each parent ]
]

{ #category : #'*Reflectivity' }
Variable >> astNodes [
	"by default we search the usingMethods for all AST nodes"
	^self usingMethods flatCollect: [ :method | 
		method variableNodes select: [: each| each variable == self ]]
]

{ #category : #'*Reflectivity' }
Variable >> availableReifications [
	^RFReification subclasses 
		select: [ :e | e entities includesAny: self class withAllSuperclasses ]
		thenCollect: #key
]

{ #category : #'*Reflectivity' }
Variable >> beforeHooks [
	^self propertyAt: #beforeHooks ifAbsentPut: OrderedCollection new
]

{ #category : #'*Reflectivity' }
Variable >> beforeLinks [
	^ self links select: [ :each | each control == #before ]
]

{ #category : #'*Reflectivity' }
Variable >> clearReflectivityAnnotations [
	self removeProperty: #preambles ifAbsent: [ ].
	self removeProperty: #beforeHooks ifAbsent: [ ].
	self removeProperty: #afterHooks ifAbsent: [ ].
	self removeProperty: #insteadHooks ifAbsent: [ ].
]

{ #category : #'*Reflectivity' }
Variable >> hasMetalink [
	^ self hasProperty: #links
]

{ #category : #'*Reflectivity' }
Variable >> hasMetalinkAfter [
	self hasMetalink
		ifFalse: [ ^ false ].
	^ self afterLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
Variable >> hasMetalinkBefore [
	self hasMetalink ifFalse: [ ^ false ].
	^ self beforeLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
Variable >> hasMetalinkInstead [
	self hasMetalink
		ifFalse: [ ^ false ].
	^ self insteadLinks isNotEmpty
]

{ #category : #'*Reflectivity' }
Variable >> insteadHooks [
	^self propertyAt: #insteadHooks ifAbsent: #()
]

{ #category : #'*Reflectivity' }
Variable >> insteadLinks [
	^ self links select: [ :each | each control == #instead ]
]

{ #category : #'*Reflectivity' }
Variable >> invalidate [
	self usingMethods do: [:method | method invalidate].
]

{ #category : #'*Reflectivity' }
Variable >> link: aMetaLink [
	(aMetaLink checkForCompatibilityWith: self) ifFalse: [ self error: 'link requests reification that can not be provided by this node' ].
	(self propertyAt: #links ifAbsentPut: [ OrderedCollection new ]) add: aMetaLink.
	aMetaLink installOnVariable: self.
	self clearReflectivityAnnotations.
	self usingMethods do: [ :method |  
		method method installLink: aMetaLink ]
]

{ #category : #'*Reflectivity' }
Variable >> links [
	^self propertyAt: #links ifAbsent: #()
]

{ #category : #'*Reflectivity' }
Variable >> postambles [
	^ self propertyAt: #postambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
Variable >> preambles [
	^ self propertyAt: #preambles ifAbsent: #()
]

{ #category : #'*Reflectivity' }
Variable >> readNodes [
	^ self astNodes select: [ :each | each isRead ]
]

{ #category : #'*Reflectivity' }
Variable >> removeLink: aMetaLink [
	"The order of operations here is critical.
	When link is removed from the method it is recompiled. But recompilation needs to look into links here in order to keep available links in the new method instance. 
	Therefore it is important to first remove the given link from the variable list and only then remove it from using methods. Otherwise the method recompilation will keep removed link in the method bytecode"	
	self links remove: aMetaLink.
	self usingMethods do: [:method | method removeLink: aMetaLink].
	
	self links ifEmpty: [ self removeProperty: #links ].
	self clearReflectivityAnnotations.
]
