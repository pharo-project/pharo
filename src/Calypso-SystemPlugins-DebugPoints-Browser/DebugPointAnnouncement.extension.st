Extension { #name : 'DebugPointAnnouncement' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
DebugPointAnnouncement >> affectsMethod: aMethod [

	^ self methods anySatisfy: [:each |  each compiledMethod == aMethod compiledMethod.  ]
]

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
DebugPointAnnouncement >> affectsMethods [
	^true
]

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
DebugPointAnnouncement >> affectsMethodsDefinedInClass: aClass [
	^ self methods
		anySatisfy: [ :each | each methodClass isNotNil and: [ each origin == aClass ] ]
]

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
DebugPointAnnouncement >> affectsMethodsDefinedInPackage: aPackage [
	^ self methods
		anySatisfy:
			[ :each | each methodClass isNotNil and: [ each package == aPackage ] ]
]

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
DebugPointAnnouncement >> canAffectResultOfMethodQuery: aMethodQuery [

	^ self methods anySatisfy: [:each | 
		aMethodQuery isAffectedByChangedMethod: each ]
]
