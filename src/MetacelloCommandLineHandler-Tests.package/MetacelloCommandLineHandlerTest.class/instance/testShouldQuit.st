tests-testing
testShouldQuit
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self
		deny: cli noQuit;
		assert: cli shouldQuit.
	
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--no-quit').
	self
		assert: cli noQuit;
		deny: cli shouldQuit