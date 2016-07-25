testing
testAsFileUrl
	self assert: 'http://localhost:80/foo/bar/doc.txt' asZnUrl asFileUrl isFile.
	self 
		assert: 'http://localhost:80/foo/bar/doc.txt' asZnUrl path 
		equals: 'http://localhost:80/foo/bar/doc.txt' asZnUrl asFileUrl path