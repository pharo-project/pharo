testing
testWindowsFileUrl
	| url |
	url := 'file://localhost/C:/users/Sven/Desktop/foo.txt' asZnUrl.
	self assert: url isFile.
	self assert: url host equals: 'localhost'.
	self assert: url pathSegments equals: #( 'C:' 'users' 'Sven' 'Desktop' 'foo.txt' ) asOrderedCollection 