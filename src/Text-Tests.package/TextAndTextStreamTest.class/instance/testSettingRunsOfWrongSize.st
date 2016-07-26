test-errors
testSettingRunsOfWrongSize
	| aText newRuns |
	aText := 'foo' asText.
	newRuns := aText runs copyReplaceFrom: 2 to: 1 with: (Array with: TextColor red).
	self should: [aText runs: newRuns] raise: Error.