convenience
sendDoubleClickAt: index withShift: aBoolean
	| buttons |
	position := (area paragraph characterBlockForIndex: index) surface leftCenter.
	buttons := (aBoolean ifTrue: [8] ifFalse: [0]).
	buttons := buttons bitOr: MouseButtonEvent redButton.
	event := MouseButtonEvent basicNew 
					setType: #mouseDown
					position: position
					which: MouseButtonEvent redButton
					buttons: buttons
					hand: (HandMorph new mouseFocus: area; yourself)
					stamp: nil.
	^ area doubleClick: event