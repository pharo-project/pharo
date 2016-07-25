bordering
border: aRectangle widthRectangle: insets rule: combinationRule fillColor: aHalfTone
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of each edge of the border is determined by the four coordinates 
	of insets. Uses aHalfTone and combinationRule for drawing the border."

	(aRectangle areasOutside: (aRectangle insetBy: insets)) do:
		[:edgeStrip | self fill: edgeStrip rule: combinationRule fillColor: aHalfTone]