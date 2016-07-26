debug and other
ownerChain
	"Answer a list of objects representing the receiver and all of its owners.   The first element is the receiver, and the last one is typically the world in which the receiver resides"

	| c next |
	c := OrderedCollection with: self.
	next := self.
	[(next := next owner) notNil] whileTrue: [c add: next].
	^c asArray