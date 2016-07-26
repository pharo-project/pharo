styles
hintHideArrowStyleFor: aBrick

	aBrick ifNil: [ ^ self ].
	(aBrick valueOfProperty: #brickSpotterIsReveal ifAbsent: [ false ])
		ifFalse: [ ^ self ].
	aBrick setProperty: #brickSpotterIsReveal toValue: false.
	
	(aBrick hasProperty: #brickSpotterBackupColor)
		ifTrue: [ aBrick color: (aBrick valueOfProperty: #brickSpotterBackupColor) ].
		
	aBrick changed