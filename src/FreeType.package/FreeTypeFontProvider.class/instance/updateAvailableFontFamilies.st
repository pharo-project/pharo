loading and updating
updateAvailableFontFamilies
	'Calculating available font families' displayProgressFrom: 0 to: 1 during:[ :bar |
		"self removeUnavailableTextStyles."
		"self addTextStylesWithPointSizes: #(8 10 12 15 24)."
		tempFileInfos := nil.
		self buildFamilies.
		tempFamilies := nil ].