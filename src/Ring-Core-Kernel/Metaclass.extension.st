Extension { #name : #Metaclass }

{ #category : #'*Ring-Core-Kernel' }
Metaclass >> asFullRingDefinition [

	^ self theNonMetaClass asFullRingDefinition classSide
]

{ #category : #'*Ring-Core-Kernel' }
Metaclass >> asRingDefinition [

	^ self theNonMetaClass asRingDefinition classSide
]
