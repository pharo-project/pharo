user interface
maxNameWidthForFont: aFont 
	| max |
	max := 0.
	self domainValuesLabels
		do: [:v | max := max max: (aFont widthOfStringOrText: v)].
	^ max 