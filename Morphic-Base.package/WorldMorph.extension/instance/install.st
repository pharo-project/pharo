*Morphic-Base
install
	"this method was used to be invoked from onPrimitiveError: 
	the problem is that it cannot be used to recreate the world instance.
	For this have a look class side installNewWorld."
	owner := nil.	"since we may have been inside another world previously"
	ActiveWorld := self.
	World := self.
	ActiveHand := self hands first.	"default"
	ActiveEvent := nil.
	submorphs do: [:ss | ss owner isNil ifTrue: [ss privateOwner: self]].
	"Transcript that was in outPointers and then got deleted."
	self viewBox: Display boundingBox.
	Sensor flushAllButDandDEvents.
	worldState handsDo: [:h | h initForEvents].
	self borderWidth: 0.	"default"
	SystemWindow noteTopWindowIn: self.
	self displayWorldSafely