Extension { #name : #TApplyingOnClassSide }

{ #category : #'*Ring-Core-Kernel' }
TApplyingOnClassSide >> asFullRingDefinition [

	^ self instanceSide asFullRingDefinition classSide
]

{ #category : #'*Ring-Core-Kernel' }
TApplyingOnClassSide >> asRingDefinition [

	^ self instanceSide asRingDefinition classSide
]
