*SUnit-UITesting
simulateClickWith: buttons position: position
	"Did you know there's #simulateClick (left), #simulateMiddleClick, and #simulateRightClick?
	buttons - look at MouseEvent's class-side for button types"

	| noButtons hand |
	noButtons := 0.
	hand := HandMorph new mouseFocus: self; yourself.
	hand privateOwner: self.
	{ #mouseDown->buttons. #mouseUp->noButtons } do: [ :type |
		hand handleEvent: (MouseButtonEvent new 
			setType: type key
			position: position "Some Morphs report local coords"
			which: (noButtons bitXor: buttons)
			buttons: type value
			hand: hand
			stamp: Time millisecondClockValue) ].