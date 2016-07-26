*Polymorph-Widgets
expandFullBoundsForDropShadow: aRectangle
	"Return an expanded rectangle for an eventual drop shadow."
	
	^(aRectangle expandBy: self shadowOffsetRectangle)
		quickMerge: aRectangle