converting
convertButton: osButton
	osButton = 1 ifTrue: [ ^ MouseButtonEvent redButton ].
	osButton = 2 ifTrue: [ ^ MouseButtonEvent blueButton ].
	osButton = 3 ifTrue: [ ^ MouseButtonEvent yellowButton ].