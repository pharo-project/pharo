visiting
visitJoyAxisEvent: axisEvent
	| axisValue velocity |
	axisEvent isPreferredEvent ifFalse: [ ^ self ].
	
	axisValue := self normalizeAxisValue: axisEvent value.
	velocity := example characterVelocity.
	axisEvent isLeftX ifTrue: [ 
		velocity := (axisValue * 300) @ velocity y
	].

	axisEvent isLeftY ifTrue: [ 
		"Left Y"
		velocity := velocity x @ (axisValue * 300)

	].

	example characterVelocity: velocity