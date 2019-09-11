Extension { #name : #MethodRecategorized }

{ #category : #'*Calypso-SystemQueries' }
MethodRecategorized >> affectsMethodsTaggedWith: tagName [
	^(super affectsMethodsTaggedWith: tagName) or: [ oldProtocol = tagName ]
]

{ #category : #'*Calypso-SystemQueries' }
MethodRecategorized >> shouldBeConsideredByCalypsoEnvironment [
	"Following case is needed to address problem when class with extension methods is moving into package where these methods are defined. At middle of such refactoring method can be in the state when it returns nil package which is inconsistent system state. It will break Calypso changes processing logic.
	This issue means that repackaging is not performed correctly. But repackaging will be announced with separate event MethodRepackaged. So it is safe to skip recategorization.
	In future this refactoring should be fixed. All objects should be in valid state when any event is triggered"
	^((oldProtocol beginsWith: '*') 
		and: [ self newProtocol = Protocol unclassified ]) not
]
