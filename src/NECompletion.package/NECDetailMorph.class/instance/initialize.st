initialization
initialize
	| childBounds |
	super initialize.
	
	self borderStyle: (BorderStyle color: Color gray width: 1).
	childBounds := self contentBounds.
	label := StringMorph 
		contents: ''
		font: self messageFont.
	label bounds: childBounds.
	self addMorph: label.
	title := StringMorph 
		contents: ''
		font: NECMenuMorph titleFont.
	title bounds: self titleBounds.
	self addMorph: title.
	description := TextMorph new.
	description autoFit: false.
	description bounds: self descriptionBounds.
	description borderWidth: 0.
	self addMorph: description