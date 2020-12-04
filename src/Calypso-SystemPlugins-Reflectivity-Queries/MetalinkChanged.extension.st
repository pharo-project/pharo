Extension { #name : #MetalinkChanged }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Queries' }
MetalinkChanged >> canAffectResultOfMethodQuery: aMethodQuery [

	^link methods anySatisfy: [:each | 
		aMethodQuery isAffectedByChangedMethod: each ]
]
