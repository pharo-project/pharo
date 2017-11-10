Extension { #name : #Metaclass }

{ #category : #'*Ring-Core-Kernel' }
Metaclass >> asFullRingDefinition [

	^ self theNonMetaClass asFullRingDefinition theMetaClass
]

{ #category : #'*Ring-Core-Kernel' }
Metaclass >> asRingDefinition [

	^ self theNonMetaClass asRingDefinition theMetaClass
]
