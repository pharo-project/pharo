blinking
hideShow
	self textArea canChangeText 
		ifFalse: [ ^ self beNotVisible ].
	self stayVisible 
		ifTrue: [ ^ self beVisible ].
	self isBlinkingCursor
		ifTrue: [ 
			self switchColor.
			self addAlarm ]
		ifFalse: [ self beVisible ]