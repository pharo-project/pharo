testing
testConvenienceMethods
	| baseUrl |
	baseUrl := 'http://api.host.com' asZnUrl.
	self 
		assert: baseUrl 
		equals: 'http://api.host.com' asZnUrl.
	self 
		assert: baseUrl / 'doc' / 'file.html'
		equals: 'http://api.host.com/doc/file.html' asZnUrl.
	self 
		assert: baseUrl / 'doc/file.html'
		equals: 'http://api.host.com/doc/file.html' asZnUrl.
	self 
		assert: baseUrl / #( 'doc' 'file.html' )
		equals: 'http://api.host.com/doc/file.html' asZnUrl.
	self 
		assert: baseUrl / 'doc/file.html' asZnUrl
		equals: 'http://api.host.com/doc/file.html' asZnUrl.
	self 
		assert: (baseUrl / 'search' ? (#q -> 'Smalltalk') & (#lang -> #en))
		equals: 'http://api.host.com/search?q=Smalltalk&lang=en' asZnUrl.
	self 
		assert: baseUrl 
		equals: 'http://api.host.com' asZnUrl