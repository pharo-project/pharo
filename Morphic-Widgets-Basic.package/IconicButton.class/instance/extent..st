geometry
extent: newExtent
	"Update the graphic position to keep centered."
	
	super extent: newExtent.
	graphicalMorph ifNotNil: [graphicalMorph position: self center - (graphicalMorph extent // 2)]