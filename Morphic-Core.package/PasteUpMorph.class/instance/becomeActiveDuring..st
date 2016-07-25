initialization
becomeActiveDuring: aBlock
	"Make the receiver the ActiveWorld during the evaluation of aBlock.
	Note that this method does deliberately *not* use #ensure: to prevent
	re-installation of the world on project switches."
	| priorWorld priorHand priorEvent |
	priorWorld := ActiveWorld.
	priorHand := ActiveHand.
	priorEvent := ActiveEvent.
	ActiveWorld := self.
	ActiveHand := self hands first. "default"
	ActiveEvent := nil. "not in event cycle"
	aBlock
		on: Error
		do: [:ex | 
			ActiveWorld := priorWorld.
			ActiveEvent := priorEvent.
			ActiveHand := priorHand.
			ex pass]