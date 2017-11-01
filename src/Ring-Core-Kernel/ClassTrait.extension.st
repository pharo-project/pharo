Extension { #name : #ClassTrait }

{ #category : #'*Ring-Core-Kernel' }
ClassTrait >> asFullRingDefinition [

	^ self theNonMetaClass asFullRingDefinition theMetaClass
]

{ #category : #'*Ring-Core-Kernel' }
ClassTrait >> asRingDefinition [

	^ self theNonMetaClass asRingDefinition theMetaClass
]
