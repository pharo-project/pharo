tests-testing
testHasGroups
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--groups=core,tests').
	self assert: cli hasGroups.
	
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self deny: cli hasGroups.