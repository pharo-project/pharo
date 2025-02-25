Extension { #name : 'MethodRecategorized' }

{ #category : '*Calypso-SystemQueries' }
MethodRecategorized >> affectsMethodsInProtocol: protocol [

	^ (super affectsMethodsInProtocol: protocol) or: [ self oldProtocol name = protocol ]
]

{ #category : '*Calypso-SystemQueries' }
MethodRecategorized >> shouldBeConsideredByCalypsoEnvironment [
	"Following case is needed to address problem when class with extension methods is moving into package where these methods are defined. At middle of such refactoring method can be in the state when it returns nil package which is inconsistent system state. It will break Calypso changes processing logic.
	This issue means that repackaging is not performed correctly. But repackaging will be announced with separate event MethodRepackaged. So it is safe to skip recategorization.
	In future this refactoring should be fixed. All objects should be in valid state when any event is triggered"

	"False for cases when brosing unloaded code"
	oldProtocol isSymbol
		ifTrue: [ ^ false ].
	"The nil check happens when we remove the method."
	^ (oldProtocol isExtensionProtocol and: [ self newProtocol isNotNil and: [ self newProtocol isUnclassifiedProtocol ] ]) not
]
