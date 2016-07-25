initialization
initialize
	super initialize.
 	self  borderStyle: BorderStyle thinGray.
	
	self setProperty: #autoFitContents toValue: false.
 
	self initializeLabelAttributes.
	self initializeContentsAttributes.
	self vanishDelay: self defaultVanishDelay.
	self label: 'A cool title' contents: 'Here an important message'.
	dismissHandle := self createDismissHandle.
	self addMorph: dismissHandle