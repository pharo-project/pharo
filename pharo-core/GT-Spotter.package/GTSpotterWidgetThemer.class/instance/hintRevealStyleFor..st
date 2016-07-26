styles
hintRevealStyleFor: aBrick
	
	SmalltalkImage current vm canHandleShiftPressed
		ifTrue: [ ^ self ].
	
	aBrick ifNil: [ ^ self ].
	(aBrick valueOfProperty: #brickSpotterIsReveal ifAbsent: [ false ])
		ifTrue: [ ^ self ].
	
	aBrick setProperty: #brickSpotterBackupColor toValue: aBrick color.
	aBrick setProperty: #brickSpotterBackupBorder toValue: aBrick border.
	aBrick setProperty: #brickSpotterIsReveal toValue: true.
	
	aBrick color: self themer popupHelpThemer backgroundColor.
	aBrick border: GLMBrickBorder new.
	aBrick borderColor: (self themer popupHelpThemer borderColor alpha: 0.6).
	aBrick borderWidth: self themer popupHelpThemer borderWidth.
	
	aBrick changed