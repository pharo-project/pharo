Extension { #name : #TApplyingOnClassSide }

{ #category : #'*Ring-Core-Kernel' }
TApplyingOnClassSide >> asFullRingDefinition [

	^ self theNonMetaClass asFullRingDefinition theMetaClass
]

{ #category : #'*Ring-Core-Kernel' }
TApplyingOnClassSide >> asRingDefinition [

	^ self theNonMetaClass asRingDefinition theMetaClass
]
