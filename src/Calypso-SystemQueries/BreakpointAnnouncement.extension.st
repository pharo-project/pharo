Extension { #name : 'BreakpointAnnouncement' }

{ #category : '*Calypso-SystemQueries' }
BreakpointAnnouncement >> affectsMethod: aMethod [

	^ self link methods anySatisfy: [:each | each compiledMethod == aMethod compiledMethod ]
]

{ #category : '*Calypso-SystemQueries' }
BreakpointAnnouncement >> affectsMethods [
	^true
]

{ #category : '*Calypso-SystemQueries' }
BreakpointAnnouncement >> affectsMethodsDefinedInClass: aClass [
	^ self link methods
		anySatisfy: [ :each | each methodClass isNotNil and: [ each origin == aClass ] ]
]

{ #category : '*Calypso-SystemQueries' }
BreakpointAnnouncement >> affectsMethodsDefinedInPackage: aPackage [
	^ self link methods
		anySatisfy:
			[ :each | each methodClass isNotNil and: [ each package == aPackage ] ]
]
