tests-accessing
testSubCommand
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli subCommand equals: 'install'.
	
	cli := self command: #('foo' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self should: [ cli subCommand ] raise: Exit