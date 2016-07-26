tests
testReferenceTo
	|path|
	"use a relative path since absolute path behavior differs on mac, linux vs win native filesystems"
	path := Path * 'a' / 'b'.
	self assert: (filesystem referenceTo: 'a/b') path = path.