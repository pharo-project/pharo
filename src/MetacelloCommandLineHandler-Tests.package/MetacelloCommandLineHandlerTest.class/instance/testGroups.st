tests-accessing
testGroups
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--groups=core,tests').
	self assertCollection: cli groups hasSameElements: #('core' 'tests')