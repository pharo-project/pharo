halos and balloon help
mouseDownOnHelpHandle: anEvent
	"The mouse went down in the show-balloon handle"
	
	| str |
	anEvent shiftPressed ifTrue: [^ self editBalloonHelpText].
	str := self balloonText.
	str ifNil: [str := self noHelpString].
	self showBalloon: str hand: anEvent hand.
