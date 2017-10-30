tests-testing
testHasVersion
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--version=stable').
	self assert: cli hasVersion.
	
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self deny: cli hasVersion.