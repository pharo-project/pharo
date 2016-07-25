query
allImplementedMessages
	"Answer a Collection of all the messages that are implemented in the system."
	| messages |
	messages := IdentitySet new.
	self allBehaviorsDo: [:each | messages addAll: each selectors].
	^ messages