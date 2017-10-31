tests-accessing
testVersion
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--version=stable').
	self assert: cli version equals: 'stable'