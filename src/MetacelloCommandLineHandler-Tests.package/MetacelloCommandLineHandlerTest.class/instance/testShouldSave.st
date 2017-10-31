tests-testing
testShouldSave
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self
		deny: cli noSave;
		assert: cli shouldSave.
		
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--no-save').
	self
		assert: cli noSave;
		deny: cli shouldSave