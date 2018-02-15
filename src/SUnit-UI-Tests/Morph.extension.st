Extension { #name : #Morph }

{ #category : #'*SUnit-UI-Tests' }
Morph >> simulateClick [

	self simulateClickWith: MouseEvent redButton.
]

{ #category : #'*SUnit-UI-Tests' }
Morph >> simulateClickWith: buttons [
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
]

{ #category : #'*SUnit-UI-Tests' }
Morph >> simulateClickWith: buttons position: position [
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
]

{ #category : #'*SUnit-UI-Tests' }
Morph >> simulateMiddleClick [

	self simulateClickWith: MouseEvent blueButton.
]

{ #category : #'*SUnit-UI-Tests' }
Morph >> simulateRightClick [

	self simulateClickWith: MouseEvent yellowButton.
]
