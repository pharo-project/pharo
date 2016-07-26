tests
testHome
	| home |
	home := self assertOriginResolves: #home.
	self assert: home isDirectory