initialization
createTitle
	| titleString transformationMorph |
	titleString := context model title ifNil: [ ^ self ].
	
	titleStringMorph := StringMorph new.
	titleStringMorph 
		contents: titleString;
		font: self class messageFont.
		
	transformationMorph := TransformationMorph new.
	transformationMorph rotationDegrees: -90.0;
		offset: self position negated - (0 @ (titleStringMorph width + 12));
		addMorph: titleStringMorph.

	self addMorph: transformationMorph.
	self resize.