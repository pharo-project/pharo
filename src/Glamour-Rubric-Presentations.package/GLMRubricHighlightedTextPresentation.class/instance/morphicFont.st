accessing
morphicFont
	^ self font 
		ifNil: [ StandardFonts defaultFont  ]
		ifNotNil: [ :f | f glamourValue: self entity ]