Class {
	#name : #ZnUrlTests,
	#superclass : #TestCase,
	#category : #'Zinc-Resource-Meta-Tests'
}

{ #category : #testing }
ZnUrlTests >> testAsFileUrl [
	self assert: 'http://localhost:80/foo/bar/doc.txt' asZnUrl asFileUrl isFile.
	self 
		assert: 'http://localhost:80/foo/bar/doc.txt' asZnUrl path 
		equals: 'http://localhost:80/foo/bar/doc.txt' asZnUrl asFileUrl path
]

{ #category : #testing }
ZnUrlTests >> testAsRelativeUrl [
	| url |
	url := 'http://api.foo.com:8080/foo/bar.txt' asZnUrl.
	self assert: url hasHost.
	self assert: url hasScheme.
	self assert: url hasPort.
	self assert: url path equals: 'foo/bar.txt'.
	url := url asRelativeUrl.
	self deny: url hasHost.
	self deny: url hasScheme.
	self deny: url hasPort.
	self assert: url path equals: 'foo/bar.txt'.
	self assert: url asString equals: '/foo/bar.txt'
]

{ #category : #testing }
ZnUrlTests >> testAuthority [
	self assert: 'http://localhost:8080/foo/bar/doc.txt' asZnUrl authority = 'localhost:8080'.
	self assert: 'http://www.google.com?q=Smalltalk' asZnUrl authority = 'www.google.com'
]

{ #category : #testing }
ZnUrlTests >> testBogusBackups [
	self assert: '/../../../' asZnUrl isSlash
]

{ #category : #testing }
ZnUrlTests >> testComponentSpecifcEncoding [
	"In a URL the safe set of percent encoding depends on where we are"
	
	| url |
	url := 'http://foo.com/bar@xx=2?yy=/#$1'.
	self assert: url asZnUrl printString equals: url
]

{ #category : #testing }
ZnUrlTests >> testConvenienceMethods [
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
]

{ #category : #testing }
ZnUrlTests >> testDefaultPortUnknownScheme [
	| url |
	url := 'gitfiletree://github.com/rohitsukhwal/HelloWorld.git?protocol=git' asZnUrl.
	self assert: url scheme equals: #gitfiletree.
	self assert: url host equals: 'github.com'.
	self assert: url authority equals: 'github.com'.
	self assert: url portOrDefault isNil.
	self deny: url hasPort.
	self assert: url equals: url.
	self should: [ url enforceKnownScheme ] raise: ZnUnknownScheme 
]

{ #category : #testing }
ZnUrlTests >> testDefaultScheme [
	| url |
	url := ZnUrl fromString: 'www.example.com/foo.html' defaultScheme: #http.  
	self assert: url hasScheme.
	self assert: url scheme = #http.
	self assert: url host = 'www.example.com'.
	self assert: url pathSegments = (OrderedCollection with: 'foo.html')
	
]

{ #category : #testing }
ZnUrlTests >> testDefaultSchemeAndPort [
	| url |
	url := '' asZnUrl.
	self assert: url hasScheme not.
	self assert: url hasPort not.
	self assert: url port isNil.
	self assert: url scheme isNil.
	self assert: url schemeOrDefault = #http.
	self assert: url portOrDefault = 80
	
]

{ #category : #testing }
ZnUrlTests >> testDefaults [
	| url |
	url := '' asZnUrl asZnUrlWithDefaults.
	self assert: url hasScheme.
	self assert: url hasPort.
	self assert: url scheme = #http.
	self assert: url port = 80
	
]

{ #category : #testing }
ZnUrlTests >> testEncodedSlash [
	self assert: 'http://example.com/foo//' asZnUrl pathPrintString = '/foo//'.
	self assert: 'http://example.com/foo//bar/' asZnUrl pathPrintString = '/foo//bar/'.
	self assert: 'http://example.com/foo//bar/file.txt' asZnUrl pathPrintString = '/foo//bar/file.txt'.

	self assert: 'http://example.com/foo/%2F' asZnUrl pathPrintString = '/foo/%2F'.
	self assert: 'http://example.com/foo/%2Fbar/' asZnUrl pathPrintString = '/foo/%2Fbar/'.
	self assert: 'http://example.com/foo/%2F/bar/' asZnUrl pathPrintString = '/foo/%2F/bar/'.
	self assert: 'http://example.com/foo/%2Ffoo.txt' asZnUrl pathPrintString = '/foo/%2Ffoo.txt'.


]

{ #category : #testing }
ZnUrlTests >> testFileUrl [
	| url |
	url := 'file://localhost/users/Sven/Desktop/foo.txt' asZnUrl.
	self assert: url isFile.
	self assert: url host equals: 'localhost'.
	self assert: url pathSegments equals: #( 'users' 'Sven' 'Desktop' 'foo.txt' ) asOrderedCollection 
]

{ #category : #testing }
ZnUrlTests >> testImage [
	self assert: ZnUrl image isFile.
	self assert: ZnUrl image isFilePath.
]

{ #category : #testing }
ZnUrlTests >> testInContextOf [
	| baseUrl url |
	baseUrl := 'http://zn.stfx.eu/foo.txt' asZnUrl.
	url := ZnUrl new addPathSegment: 'bar.txt'.
	self assert: (url inContextOf: baseUrl) equals: 'http://zn.stfx.eu/bar.txt' asZnUrl.
	"The following holds for all URLs"
	self assert: (baseUrl asRelativeUrl inContextOf: baseUrl) equals: baseUrl
]

{ #category : #testing }
ZnUrlTests >> testIsSlash [
	self assert: '' asZnUrl isSlash.
	self assert: '/' asZnUrl isSlash.
	self assert: '/foo' asZnUrl isSlash not.
	self assert: 'http://host/foo' asZnUrl isSlash not.
]

{ #category : #testing }
ZnUrlTests >> testLocalHost [
	self assert: 'http://localhost:8080/foo' asZnUrl isLocalHost.
	self assert: 'http://127.0.0.1:8080/foo' asZnUrl isLocalHost.
	self assert: 'http://LOCALHOST:8080/foo' asZnUrl isLocalHost.
	self deny: 'http://zn.stx.eu' asZnUrl isLocalHost.
	self deny: '' asZnUrl isLocalHost.
]

{ #category : #testing }
ZnUrlTests >> testMailto [
	| url |
	url := 'mailto:sven@beta9.be?subject=test' asZnUrl.
	self assert: url scheme equals: #mailto.
	self assert: url mailToAddress equals: 'sven@beta9.be'.
	self assert: (url queryAt: 'subject') equals: 'test'.
	self assert: url printString equals: 'mailto:sven@beta9.be?subject=test'
]

{ #category : #testing }
ZnUrlTests >> testNoScheme [
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
]

{ #category : #testing }
ZnUrlTests >> testNoSchemeColonInPath [
	| url |
	url := '/foo:bar' asZnUrl.
	self deny: url hasScheme.
	self deny: url hasHost.
	self deny: url hasPort.
	self deny: url hasQuery.
	self deny: url hasFragment.
	self assert: url hasPath.
	self assert: url firstPathSegment equals: 'foo:bar'
]

{ #category : #testing }
ZnUrlTests >> testParsePathOnly [
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
]

{ #category : #testing }
ZnUrlTests >> testParsingEmpty [
	| url |
	url := ZnUrl fromString: ''.
	self assert: url isEmpty.
	url := ZnUrl fromString: '/'.
	self assert: url isEmpty.
	url := ZnUrl fromString: '/./foo/../'.
	self assert: url isEmpty.
	url := ZnUrl fromString: '//'.
	self deny: url isEmpty
]

{ #category : #testing }
ZnUrlTests >> testParsingEscape [
	| url |
	url := ZnUrl fromString: 'http://localhost/foo%62%61%72'.
	self assert: url firstPathSegment = 'foobar'
]

{ #category : #testing }
ZnUrlTests >> testParsingSimple [
	| url |
	url := ZnUrl fromString: 'http://www.example.com:8080/foo/bar/baz.txt?x=1&y=2#m1'.
	self assert: url scheme = #http.
	self assert: url host = 'www.example.com'.
	self assert: url port = 8080.
	self assert: url hasPath.
	self assert: url isFilePath.
	self assert: url hasQuery.
	self assert: url hasFragment.
	self assert: url fragment = 'm1'
]

{ #category : #testing }
ZnUrlTests >> testParsingWrongEscape [
	self should: [ ZnUrl fromString: 'http://foo:8080/foo%%bar' ] raise: ZnCharacterEncodingError 
]

{ #category : #testing }
ZnUrlTests >> testParsingWrongEscapeQuery [
	self should: [ ZnUrl fromString: 'http://foo:8080/foo?%%bar=1' ] raise: ZnCharacterEncodingError 
]

{ #category : #testing }
ZnUrlTests >> testParsingWrongPort [
	self should: [ ZnUrl fromString: 'http://foo:bar' ] raise: ZnPortNotANumber   
]

{ #category : #testing }
ZnUrlTests >> testParsingWrongScheme [
	| url |
	url := ZnUrl fromString: 'git://user@foo.com/bar'.
	self assert: url scheme equals: #git.
	self assert: url username equals: 'user'.
	self assert: url host equals: 'foo.com'.
	self assert: url path equals: 'bar'.
	self should: [ url enforceKnownScheme ] raise: ZnUnknownScheme 
]

{ #category : #testing }
ZnUrlTests >> testPathRemoval [
	| url |
	url := 'http://api.foo.com:8080/foo/123/bar.txt?x=1&y=2' asZnUrl.
	self assert: url path equals: 'foo/123/bar.txt'.
	url removeFirstPathSegment.
	self assert: url path equals: '123/bar.txt'.
	url removeLastPathSegment.
	self assert: url path equals: '123'.
	url clearPath.
	self deny: url hasPath.
	self assert: (url queryAt: #x) equals: '1'.
	self assert: (url queryAt: #y) equals: '2'.
	self assert: url scheme equals: #http.
	self assert: url host equals: 'api.foo.com'.
	self assert: url port equals: 8080
]

{ #category : #testing }
ZnUrlTests >> testPathSegments [
	self 
		assert: 'http://foo.com/x/y/z' asZnUrl pathSegments 
		equals: #('x' 'y' 'z') asOrderedCollection.
	self assert: 'http://foo.com/' asZnUrl pathSegments isEmpty.
	self assert: 'http://foo.com' asZnUrl pathSegments isEmpty
]

{ #category : #testing }
ZnUrlTests >> testPlus [
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

]

{ #category : #testing }
ZnUrlTests >> testPlusHandling [
	"While percent decoding, a + is translated as a space only in the context of 
	application/x-www-form-urlencoded get/post requests:
	http://en.wikipedia.org/wiki/Percent-encoding#The_application.2Fx-www-form-urlencoded_type
	ZnUrl interprets its query part as key value pairs where this translation is applicable,
	even though strictly speaking + (and =, &) are plain unreserved characters in the query part"
	
	"$+ is not special in the path part of the URL and it remains itself"
	self 
		assert: 'http://localhost/foo+bar' asZnUrl firstPathSegment 
		equals: 'foo+bar'.
	self 
		assert: 'http://localhost/foo+bar' asZnUrl printString 
		equals: 'http://localhost/foo+bar'.
	"$+ gets decoded to space in the interpreted query part of the URL, 
	and becomes an encoded space if needed"
	self 
		assert: ('http://localhost/test?q=foo+bar' asZnUrl queryAt: #q)
		equals: 'foo bar'.
	self 
		assert: 'http://localhost/test?q=foo+bar' asZnUrl printString
		equals: 'http://localhost/test?q=foo%20bar'.
	"to pass $+ as $+ in a query, it has to be encoded"
	self 
		assert: 'http://localhost/test?q=foo%2Bbar' asZnUrl printString
		equals: 'http://localhost/test?q=foo%2Bbar'
]

{ #category : #testing }
ZnUrlTests >> testPortIfAbsent [
	self assert: ('scheme://host' asZnUrl portIfAbsent: 123) equals: 123.
	self assert: ('scheme://host' asZnUrl portIfAbsent: [ 123 ]) equals: 123.
]

{ #category : #testing }
ZnUrlTests >> testPrintingSimple [
	| url |
	(url := ZnUrl new)
		scheme: #http;
		host: 'www.seaside.st';
		port: 8080;
		addPathSegment: 'example';
		addPathSegment: 'foo.html';
		queryAt: 'q' put: '100';
		fragment: 'mark'.
	self assert: url authority = 'www.seaside.st:8080'.
	self assert: url isAbsolute.
	self assert: url isFilePath.
	self assert: url printString = 'http://www.seaside.st:8080/example/foo.html?q=100#mark'.
	self assert: url file = 'foo.html'.
	self assert: url directory = 'example'.
	self assert: url pathPrintString = '/example/foo.html'.
	self assert: url pathQueryFragmentPrintString = '/example/foo.html?q=100#mark'
]

{ #category : #testing }
ZnUrlTests >> testQuery [
	| url |
	url := 'http://foo.com/test?q' asZnUrl.
	self assert: url printString = 'http://foo.com/test?q'
]

{ #category : #testing }
ZnUrlTests >> testQueryAccessing [
	| url |
	url := 'http://www.google.com/?one=1&two=2' asZnUrl.
	self assert: url hasQuery.
	self assert: url queryKeys sorted equals: #(one two).
	self assert: (url queryAt: 'two' ifAbsent: [ self fail ]) equals: '2'.
	self assert: (url queryAt: 'three' ifAbsent: [ #missing ]) equals: #missing.
	url queryAt: 'one' ifPresent: [ :value | self assert: value equals: '1' ].
	self assert: (url queryAt: 'three' ifPresent: [ :value | self fail ]) isNil
]

{ #category : #testing }
ZnUrlTests >> testQueryEncoding [
	| url |
	url := 'http://www.google.com' asZnUrl.
	url addPathSegment: 'some encoding here'.
	url queryAt: 'and some encoding' put: 'here, too#'.
	self assert: url printString = 'http://www.google.com/some%20encoding%20here?and%20some%20encoding=here,%20too%23'.
	self assert: url path =  'some encoding here'.
	self assert: (url queryAt: 'and some encoding') = 'here, too#'
	
]

{ #category : #testing }
ZnUrlTests >> testQueryEncodingExtended [
	| url |
	url := 'http://server.com/foo' asZnUrl.
	url 
		queryAt: 'one' put: '1';
		queryAt: 'equalSign' put: 'a=0';
		queryAt: 'ampersand' put: 'm&m';
		queryAt: 'questionMark' put: 'x?-1'.
	url := url asString asZnUrl.
	self assert: (url queryAt: 'one') equals: '1'.
	self assert: (url queryAt: 'equalSign') equals: 'a=0'.
	self assert: (url queryAt: 'ampersand') equals: 'm&m'.
	self assert: (url queryAt: 'questionMark') equals: 'x?-1'

]

{ #category : #testing }
ZnUrlTests >> testQueryManipulation [
	| url |
	url := 'http://www.google.com/?one=1&two=2' asZnUrl.
	url queryAt: 'three' put: '3'.
	url queryRemoveKey: 'one'.
	self assert: url queryKeys sorted = #(three two).
	self assert: (url queryAt: 'two') = '2'.
	self assert: (url queryAt: 'three') = '3'.
	url queryRemoveAll.
	self deny: url hasQuery
]

{ #category : #testing }
ZnUrlTests >> testQueryRemoveAll [
	#('http://foo.com/test' 'http://foo.com/test?')
		do: [ :each | 
			| url |
			url := 'http://foo.com/test?name=value' asZnUrl.
			self deny: url = each asZnUrl.
			url queryRemoveAll.
			self
				assert: url query isEmpty;
				assert: url = each asZnUrl ]
]

{ #category : #testing }
ZnUrlTests >> testReferenceResolution [
	"RFC 3986 Section 5"
	
	| baseUri specification result succeeded failed |
	baseUri := 'http://a/b/c/d;p?q' asZnUrl.
	specification := {
		"Examples 5.4.1 - Normal" 
		" 'g:h' -> 'g:h'. " "we do not support unknown schemes without //"
		'mailto:john@acme.com' -> 'mailto:john@acme.com'. "this we can do"
		'g' -> 'http://a/b/c/g'.
		'./g' -> 'http://a/b/c/g'.
		'g/' -> 'http://a/b/c/g/'.
		'/g' -> 'http://a/g'.
		'//g' -> 'http://g/'.  "a trailing slash is added automatically"
		'//a.com/assets/img.jpg' -> 'http://a.com/assets/img.jpg'.
		'?y' -> 'http://a/b/c/d;p?y'.
		'g?y' -> 'http://a/b/c/g?y'.
		'#s' -> 'http://a/b/c/d;p?q#s'.
		'g#s' -> 'http://a/b/c/g#s'.
		'g?y#s' -> 'http://a/b/c/g?y#s'.
		';x' -> 'http://a/b/c/;x'.
		'g;x' -> 'http://a/b/c/g;x'.
		'g;x?y#s' -> 'http://a/b/c/g;x?y#s'.
		'' -> 'http://a/b/c/d;p?q'.
		'.' -> 'http://a/b/c/'.
		'./' -> 'http://a/b/c/'.
		'..' -> 'http://a/b/'.
		'../' -> 'http://a/b/'.
		'../g' -> 'http://a/b/g'.
		'../..' -> 'http://a/'.
		'../../' -> 'http://a/'.
		'../../g' -> 'http://a/g'.
		"Examples 5.4.2 - Abnormal" 
		'../../../g' -> 'http://a/g'.
		'../../../../g' -> 'http://a/g'.
		'/./g' -> 'http://a/g'.
		'/../g' -> 'http://a/g'.
		'g.' -> 'http://a/b/c/g.'.
		'.g' -> 'http://a/b/c/.g'.
		'g..' -> 'http://a/b/c/g..'.
		'..g' -> 'http://a/b/c/..g'.
		'./../g' -> 'http://a/b/g'.
		'./g/.' -> 'http://a/b/c/g/'.
		'g/./h' -> 'http://a/b/c/g/h'.
		'g/../h' -> 'http://a/b/c/h'.
		'g;x1/./y' -> 'http://a/b/c/g;x1/y'.
		'g;x1/../y' -> 'http://a/b/c/y'.
		'g?y/./x' -> 'http://a/b/c/g?y/./x'.
		'g?y/../x' -> 'http://a/b/c/g?y/../x'.
		'g#s/./x' -> 'http://a/b/c/g#s/./x'.
		'g#s/../x' -> 'http://a/b/c/g#s/../x'.
		'http://g' -> 'http://g/' "a trailing slash is added automatically"
	}.
	result := specification withIndexCollect: [ :spec :index | | resolved success |
		resolved := baseUri withRelativeReference: spec key.
		success := resolved asString = spec value.
		{ #input -> spec key. #expected -> spec value. #index -> index.
		#resolved -> resolved. #result -> success } asDictionary ].
	succeeded := result select: [ :each | each at: #result ].
	failed := result reject: [ :each | each at: #result ].
	self assert: failed isEmpty
]

{ #category : #testing }
ZnUrlTests >> testRelative [
	| url |
	url := 'http://api.foo.com:8080' asZnUrl.
	self assert: url isAbsolute.
	url parseFrom: '/user/1?format=full'.
	self assert: url printString = 'http://api.foo.com:8080/user/1?format=full'
]

{ #category : #testing }
ZnUrlTests >> testSchemeInQuery [
	| url |
	url := '/foo/bar/file.txt?url=http://www.pharo.org' asZnUrl.
	self deny: url hasScheme.
	self deny: url hasHost.
	self assert: url isFilePath.
	self assert: (url queryAt: #url) equals: 'http://www.pharo.org'
]

{ #category : #testing }
ZnUrlTests >> testUserInfo [
	| url |
	url := 'http://john:secret@www.foo.com:8888/foo/bar.txt' asZnUrl.
	self assert: url scheme equals: #http.
	self assert: url username equals: 'john'.
	self assert: url password equals: 'secret'.
	self assert: url host equals: 'www.foo.com'.
	self assert: url port equals: 8888.
	self assert: url pathSegments asArray equals: #('foo' 'bar.txt').
	url := 'http://john@www.foo.com:8888/foo/bar.txt' asZnUrl.
	self assert: url scheme equals: #http.
	self assert: url username equals: 'john'.
	self deny: url hasPassword.
	self assert: url host equals: 'www.foo.com'.
	self assert: url port equals: 8888.
	self assert: url pathSegments asArray equals: #('foo' 'bar.txt')
]

{ #category : #testing }
ZnUrlTests >> testWindowsFileUrl [
	| url |
	url := 'file://localhost/C:/users/Sven/Desktop/foo.txt' asZnUrl.
	self assert: url isFile.
	self assert: url host equals: 'localhost'.
	self assert: url pathSegments equals: #( 'C:' 'users' 'Sven' 'Desktop' 'foo.txt' ) asOrderedCollection 
]

{ #category : #testing }
ZnUrlTests >> testWriteUrlPathQueryFragmentOfOn [
	| string |
	string := String streamContents: [ :stream |
		'http://host:7777/foo/bar/doc.txt?x=1#mark' asZnUrl printPathQueryFragmentOn:  stream ].
	self assert: string = '/foo/bar/doc.txt?x=1#mark'
]
