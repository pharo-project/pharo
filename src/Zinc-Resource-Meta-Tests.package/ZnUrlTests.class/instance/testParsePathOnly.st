testing
testParsePathOnly
	| url |
	url := '/images/foo.png?size=large#center' asZnUrl.
	self assert: url hasScheme not.
	self assert: url hasHost not.
	self assert: url hasPort not.
	self assert: url isAbsolute not.
	self assert: url pathSegments = (OrderedCollection with: 'images' with: 'foo.png' ).
	self assert: url firstPathSegment = 'images'.
	self assert: url lastPathSegment = 'foo.png'.
	self assert: url hasQuery.
	self assert: (url queryAt: 'size') = 'large'.
	self assert: url hasFragment.
	self assert: url fragment = 'center'  