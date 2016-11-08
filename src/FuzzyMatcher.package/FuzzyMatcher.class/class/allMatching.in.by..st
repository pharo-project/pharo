utilities api
allMatching: aPattern in: aCollection by: aBlockReturningString

	| matcher |
	
	matcher := self pattern: aPattern.

	^ aCollection select: [ :each | matcher matches: (aBlockReturningString value: each) ]