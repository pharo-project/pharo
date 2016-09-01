accessing
realValue
	^ (self class environment at: self fontClassName)
				familyName: self familyName 
				size: self pointSize
				emphasized: self emphasized