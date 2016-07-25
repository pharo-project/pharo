accessing
pointSizes
	^ fontArray collect:
		[:x | x pointSize]

  "TextStyle default fontNamesWithPointSizes"