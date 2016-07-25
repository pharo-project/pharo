testing
testPlus
	| base |
	base := 'myscheme://user:password@localhost:8888/v1/bar' asZnUrl.
	
	self assert: (base + 'foo') scheme equals: #myscheme.
	self assert: (base + 'foo') host equals: 'localhost'.
	self assert: (base + 'foo') port equals: 8888.
	self assert: (base + 'foo') username equals: 'user'.
	self assert: (base + 'foo') password equals: 'password'.
	self assert: (base + 'foo') directory equals: 'v1'.
	self assert: (base + 'foo') file equals: 'foo'.
	
	self assert: (base + '/foo') path equals: 'foo'.
	self assert: (base + '/foo') host equals: 'localhost'.
	self assert: (base + '/foo') port equals: 8888.
	
	self assert: (base + '//foo') scheme equals: #myscheme.
	self assert: (base + '//foo') host equals: 'foo'.
	"According to RFC 3986 section 5.2.2 the whole authority is replaced"
	self deny: (base + '//foo') hasPort
	"Use #portIfAbsent: to use a default port"
