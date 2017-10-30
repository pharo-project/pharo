tests-accessing
testRepositoryUrl
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli repositoryUrl equals: 'github://juliendelplanque/MineSweeper/repository'.