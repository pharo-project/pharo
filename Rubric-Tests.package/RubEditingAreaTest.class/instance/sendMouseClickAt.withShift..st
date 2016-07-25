convenience
sendMouseClickAt: index withShift: aBoolean
	| buttons |
	position := (area paragraph characterBlockForIndex: index) surface leftCenter.
	buttons := (aBoolean ifTrue: [8] ifFalse: [0]).
	buttons := buttons bitOr: MouseButtonEvent redButton.
	area simulateClickWith: buttons position: position.
	