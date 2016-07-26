comparing
= anOrientedFillStyle
	"Answer whether equal."

	^super = anOrientedFillStyle
		and: [self form = anOrientedFillStyle form]