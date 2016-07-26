accessing
numberSelectors
	| total |
	total := 0.
	self selectorsDo: [:sel | total := total + 1 ].
	^total