initialization
initialize
	super initialize.
	self layoutPolicy: TableLayout new.
	self listCentering: #center.
	self width: 300.
	self height: 25.
	self color: (Smalltalk ui theme textEditorDisabledFillStyleFor: self).
	self borderStyle: (Smalltalk ui theme textEditorDisabledBorderStyleFor: self).
	
	labelMorph := StringMorph contents: ''.
	self addMorph: (labelMorph).
	
	edited := false.
	focused := false.
	self initializeKeystrokes.
	