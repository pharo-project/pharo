tests
testInstance
	| entry |
	entry := NECInstVarEntry contents: 'abc'.
	self assert: entry isInstance