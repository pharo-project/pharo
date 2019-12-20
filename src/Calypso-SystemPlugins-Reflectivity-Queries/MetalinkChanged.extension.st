Extension { #name : #MetalinkChanged }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Queries' }
MetalinkChanged >> affectsMethod: aMethod [

	^link methods anySatisfy: [:each | each compiledMethod == aMethod compiledMethod ]
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Queries' }
MetalinkChanged >> affectsMethods [
	^true
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Queries' }
MetalinkChanged >> affectsMethodsDefinedInClass: aClass [
	^ link methods
		anySatisfy: [ :each | each methodClass isNotNil and: [ each origin == aClass ] ]
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Queries' }
MetalinkChanged >> affectsMethodsDefinedInPackage: aPackage [
	^ link methods
		anySatisfy:
			[ :each | each methodClass isNotNil and: [ each package == aPackage ] ]
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Queries' }
MetalinkChanged >> canAffectResultOfMethodQuery: aMethodQuery [

	^link methods anySatisfy: [:each | 
		aMethodQuery isAffectedByChangedMethod: each ]
]
