accessing
fontListStrings

	| isUpdateRequired |
	isUpdateRequired := fontListStrings isNil or: [fontList size ~= fontListStrings size].
	isUpdateRequired ifTrue: [
		fontListStrings := self fontList collect: [:each | each familyName]].
	^ fontListStrings