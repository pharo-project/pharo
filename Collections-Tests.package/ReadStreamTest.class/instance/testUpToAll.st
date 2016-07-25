tests
testUpToAll

	self assert: (self streamOn: 'abcdefgh' upToAll: 'cd') = 'ab'.
	self assert: (self streamOn: 'abcdefgh' upToAll: 'cd' upToAll: 'gh') = 'ef'.

	self assert: (self streamOn: '' upToAll: '') = ''.

	self assert: (self streamOn: 'a' upToAll: '') = ''.
	self assert: (self streamOn: 'a' upToAll: 'a') = ''.
	self assert: (self streamOn: 'a' upToAll: 'b') = 'a'.

	self assert: (self streamOn: 'ab' upToAll: '') = ''.
	self assert: (self streamOn: 'ab' upToAll: 'a') = ''.
	self assert: (self streamOn: 'ab' upToAll: 'b') = 'a'.
	self assert: (self streamOn: 'ab' upToAll: 'c') = 'ab'.
	self assert: (self streamOn: 'ab' upToAll: 'ab') = ''.

	self assert: (self streamOn: 'abc' upToAll: '') = ''.
	self assert: (self streamOn: 'abc' upToAll: 'a') = ''.
	self assert: (self streamOn: 'abc' upToAll: 'b') = 'a'.
	self assert: (self streamOn: 'abc' upToAll: 'c') = 'ab'.
	self assert: (self streamOn: 'abc' upToAll: 'd') = 'abc'.
	self assert: (self streamOn: 'abc' upToAll: 'ab') = ''.
	self assert: (self streamOn: 'abc' upToAll: 'bc') = 'a'.
	self assert: (self streamOn: 'abc' upToAll: 'cd') = 'abc'.
