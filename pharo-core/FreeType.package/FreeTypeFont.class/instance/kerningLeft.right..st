measuring
kerningLeft: leftChar right: rightChar
	^self isSubPixelPositioned
		ifTrue: [self linearKerningLeft: leftChar right: rightChar]
		ifFalse:[self hintedKerningLeft: leftChar right: rightChar]