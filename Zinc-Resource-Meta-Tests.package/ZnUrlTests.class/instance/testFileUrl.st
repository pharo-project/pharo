testing
testFileUrl
	| url |
	url := 'file://localhost/users/Sven/Desktop/foo.txt' asZnUrl.
	self assert: url isFile.
	self assert: url host equals: 'localhost'.
	self assert: url pathSegments equals: #( 'users' 'Sven' 'Desktop' 'foo.txt' ) asOrderedCollection 