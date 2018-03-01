Extension { #name : #ClassTrait }

{ #category : #'*Ring-Core-Kernel' }
ClassTrait >> asFullRingDefinition [

	^ self instanceSide asFullRingDefinition classSide
]

{ #category : #'*Ring-Core-Kernel' }
ClassTrait >> asRingDefinition [

	^ self instanceSide asRingDefinition classSide
]
