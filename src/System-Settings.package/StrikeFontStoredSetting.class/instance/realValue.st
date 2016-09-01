accessing
realValue
	^ (self class environment at: self fontClassName)
			familyName: self familyName
			pointSize: self pointSize
			emphasized: self emphasized