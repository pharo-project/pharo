styles
hintHideStyleFor: aBrick

	aBrick ifNil: [ ^ self ].
	(aBrick valueOfProperty: #brickSpotterIsReveal ifAbsent: [ false ])
		ifFalse: [ ^ self ].
	aBrick setProperty: #brickSpotterIsReveal toValue: false.
	
	(aBrick hasProperty: #brickSpotterBackupColor)
		ifTrue: [ aBrick color: (aBrick valueOfProperty: #brickSpotterBackupColor) ].
		
	(aBrick hasProperty: #brickSpotterBackupBorder)
		ifTrue: [ aBrick border: (aBrick valueOfProperty: #brickSpotterBackupBorder) ].
		
	aBrick changed