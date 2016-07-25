private
isPositionAtPatternArg
	| currentRange |
	currentRange := self currentRange.
	^ currentRange notNil and: [ currentRange type = #patternArg ]