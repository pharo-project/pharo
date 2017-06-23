accessing
branch
	"The branch variable and this accessor are useful when you do not have a real git repository yet. Once created it might be safer to use #currentBranch instead."
	^ branch ifNil: [ branch := self lookupHead ]
