*SUnit-UITesting
simulateClickWith: buttons
	"Did you know there's #simulateClick (left), #simulateMiddleClick, and #simulateRightClick?
	buttons - look at MouseEvent's class-side for button types"

	| noButtons |
	noButtons := 0.
	{ #mouseDown->buttons. #mouseUp->noButtons } do: [ :type |
		self activeHand handleEvent: (MouseButtonEvent new 
			setType: type key
			position: (self pointInWorld: self center) "Some Morphs report local coords"
			which: (noButtons bitXor: buttons)
			buttons: type value
			hand: self activeHand
			stamp: Time millisecondClockValue) ].