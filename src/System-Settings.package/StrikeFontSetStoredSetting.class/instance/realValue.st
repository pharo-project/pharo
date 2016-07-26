accessing
realValue
	^ self fontClassName asClass
				familyName: self familyName 
				size: self pointSize
				emphasized: self emphasized