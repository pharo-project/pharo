Extension { #name : #Metaclass }

{ #category : #'*Ring-Core-Kernel' }
Metaclass >> asFullRingDefinition [

	^ self instanceSide asFullRingDefinition classSide
]

{ #category : #'*Ring-Core-Kernel' }
Metaclass >> asRingDefinition [

	^ self instanceSide asRingDefinition classSide
]
