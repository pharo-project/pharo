execute
execute

	| answer |
	answer := self uiManager request: 'give a new baseline number (the prefix "-baseline" will be added)'.
	answer ifNil: [ ^ self ].

	self copyBaselineWithNumber: answer

