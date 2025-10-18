Extension { #name : 'ClassRepackaged' }

{ #category : '*Calypso-SystemQueries' }
ClassRepackaged >> affectsMethods [
	"methods belong to class which could be represented by tools as part of method"
	^true
]

{ #category : '*Calypso-SystemQueries' }
ClassRepackaged >> affectsMethodsDefinedInClass: aClass [

	^classRepackaged == aClass
]

{ #category : '*Calypso-SystemQueries' }
ClassRepackaged >> affectsMethodsDefinedInPackage: aPackage [

	^self oldPackage == aPackage or: [ self newPackage == aPackage ]
]

{ #category : '*Calypso-SystemQueries' }
ClassRepackaged >> affectsMethodsInProtocol: protocol [

	^ classRepackaged protocolNames includes: protocol
]

{ #category : '*Calypso-SystemQueries' }
ClassRepackaged >> canAffectResultOfMethodQuery: aMethodQuery [

	^true
]
