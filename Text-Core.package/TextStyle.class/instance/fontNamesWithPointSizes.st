accessing
fontNamesWithPointSizes
	^ fontArray collect:
		[:x | x fontNameWithPointSize]

  "TextStyle default fontNamesWithPointSizes"