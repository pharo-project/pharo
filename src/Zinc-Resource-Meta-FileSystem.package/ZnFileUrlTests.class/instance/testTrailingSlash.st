testing
testTrailingSlash
	| fileReference1 fileReference2 fileUrl1 fileUrl2 |
	
	fileReference1 := '/foo/bar' asFileReference.
	fileReference2 := '/foo/bar/' asFileReference.
	self assert: fileReference1 equals: fileReference2.
	self assert: fileReference1 asZnUrl equals: fileReference2 asZnUrl.
	
	fileUrl1 := 'file:///foo/bar' asZnUrl.
	fileUrl2 := 'file:///foo/bar/' asZnUrl.
	self deny: fileUrl1 = fileUrl2.
	self assert: fileUrl1 asFileReference equals: fileUrl2 asFileReference