kerning
kerningLeft: leftCharacter right: rightCharacter
	[^self primGetKerningLeft: (self primGetCharIndex: leftCharacter asInteger)  
		right: (self primGetCharIndex: rightCharacter asInteger) 
	] on: FT2Error do:[:e | ^0@0]