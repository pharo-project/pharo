Extension { #name : #Metaclass }

{ #category : #'*Ring-Definitions-Core' }
Metaclass >> asFullRingDefinition [

	^ self instanceSide asFullRingDefinition classSide
]

{ #category : #'*Ring-Definitions-Core' }
Metaclass >> asRingDefinition [

	^ self instanceSide asRingDefinition classSide
]
