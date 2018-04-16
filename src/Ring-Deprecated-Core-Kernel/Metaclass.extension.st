Extension { #name : #Metaclass }

{ #category : #'*Ring-Deprecated-Core-Kernel' }
Metaclass >> asFullRingDefinition [

	^ self instanceSide asFullRingDefinition classSide
]

{ #category : #'*Ring-Deprecated-Core-Kernel' }
Metaclass >> asRingDefinition [

	^ self instanceSide asRingDefinition classSide
]
