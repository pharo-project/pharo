*Polymorph-Widgets
optimalExtent
	"Answer the submorphBounds extent plus twice our border width."
	
	^self submorphBounds extent + (self borderWidth * 2)