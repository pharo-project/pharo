Extension { #name : 'ClassRemoved' }

{ #category : '*Calypso-SystemQueries' }
ClassRemoved >> affectsMethod: aMethod [
	^classRemoved == aMethod origin
]

{ #category : '*Calypso-SystemQueries' }
ClassRemoved >> affectsMethods [
	"methods are removed together with removed class"
	^true
]

{ #category : '*Calypso-SystemQueries' }
ClassRemoved >> affectsMethodsDefinedInClass: aClass [

	^classRemoved == aClass
]

{ #category : '*Calypso-SystemQueries' }
ClassRemoved >> affectsMethodsDefinedInPackage: aPackage [

	^ self packageAffected == aPackage
]

{ #category : '*Calypso-SystemQueries' }
ClassRemoved >> affectsMethodsInProtocol: protocol [

	^ classRemoved protocolNames includes: protocol
]

{ #category : '*Calypso-SystemQueries' }
ClassRemoved >> canAffectResultOfMethodQuery: aMethodQuery [
	^true
]
