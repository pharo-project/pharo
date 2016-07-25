testing
testNoScheme
	"When there is no scheme, there is no host:port interpretation, 
	but instead the input is seen a a path - externalize this in test code"
	
	#('foo' 'localhost' 'www.foo.com' 'foo.txt' 'foo:1')
		do: [ :input | 
			| url |
			url := input asZnUrl.
			self deny: url hasScheme.
			self deny: url hasHost.
			self deny: url hasPort.
			self deny: url hasQuery.
			self deny: url hasFragment.
			self assert: url hasPath.
			self assert: url isFilePath.
			self assert: url firstPathSegment equals: input ].

	#('/dir/foo.txt' 'dir/foo.txt')
		do: [ :input | 
			| url |
			url := input asZnUrl.
			self deny: url hasScheme.
			self deny: url hasHost.
			self deny: url hasPort.
			self deny: url hasQuery.
			self deny: url hasFragment.
			self assert: url hasPath.
			self assert: url isFilePath.
			self assert: url pathSegments equals: #('dir' 'foo.txt') asOrderedCollection ]