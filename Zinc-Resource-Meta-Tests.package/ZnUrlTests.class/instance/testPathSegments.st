testing
testPathSegments
	self 
		assert: 'http://foo.com/x/y/z' asZnUrl pathSegments 
		equals: #('x' 'y' 'z') asOrderedCollection.
	self assert: 'http://foo.com/' asZnUrl pathSegments isEmpty.
	self assert: 'http://foo.com' asZnUrl pathSegments isEmpty