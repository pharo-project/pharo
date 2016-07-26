expression types
variable

	| varName varStart varEnd |
	varStart := self startOfNextToken + requestorOffset.
	varName := self advance.
	varEnd := self endOfLastToken + requestorOffset.
	^ encoder encodeVariable: varName
		sourceRange: (varStart to: varEnd)
		ifUnknown: [self correctVariable: varName interval: (varStart to: varEnd)]