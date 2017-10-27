fill-styles
separatorFillStyleFor: aSeparator
	"Return the separator fillStyle for the given separator."
	
	^ SolidFillStyle color: (self glamorousBaseColorFor: aSeparator) darker