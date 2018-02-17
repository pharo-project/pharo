Extension { #name : #ClassTrait }

{ #category : #'*Ring-Core-Kernel' }
ClassTrait >> asFullRingDefinition [

	^ self instanceSide asFullRingDefinition theMetaClass
]

{ #category : #'*Ring-Core-Kernel' }
ClassTrait >> asRingDefinition [

	^ self instanceSide asRingDefinition theMetaClass
]
