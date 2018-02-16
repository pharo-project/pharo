Extension { #name : #TApplyingOnClassSide }

{ #category : #'*Ring-Core-Kernel' }
TApplyingOnClassSide >> asFullRingDefinition [

	^ self theNonMetaClass asFullRingDefinition classSide
]

{ #category : #'*Ring-Core-Kernel' }
TApplyingOnClassSide >> asRingDefinition [

	^ self theNonMetaClass asRingDefinition classSide
]
