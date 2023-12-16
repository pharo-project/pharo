Extension { #name : 'MetalinkChanged' }

{ #category : '*Calypso-SystemQueries' }
MetalinkChanged >> affectsMethod: aMethod [

	^link methods anySatisfy: [:each | each compiledMethod == aMethod compiledMethod ]
]

{ #category : '*Calypso-SystemQueries' }
MetalinkChanged >> affectsMethods [
	^true
]

{ #category : '*Calypso-SystemQueries' }
MetalinkChanged >> affectsMethodsDefinedInClass: aClass [
	^ link methods
		anySatisfy: [ :each | each methodClass isNotNil and: [ each origin == aClass ] ]
]

{ #category : '*Calypso-SystemQueries' }
MetalinkChanged >> affectsMethodsDefinedInPackage: aPackage [
	^ link methods
		anySatisfy:
			[ :each | each methodClass isNotNil and: [ each package == aPackage ] ]
]
