Class {
	#name : #ZnClientTest,
	#superclass : #AbstractEnvironmentTestCase,
	#category : #'Zinc-Tests'
}

{ #category : #accessing }
ZnClientTest >> smallHtmlUrl [
	^ 'http://zn.stfx.eu/zn/small.html' asZnUrl
]

{ #category : #accessing }
ZnClientTest >> t3EasyGeoIPUrl [
	^ 'http://easy.t3-platform.net/rest/geo-ip' asZnUrl
]

{ #category : #testing }
ZnClientTest >> testConnectionCloseOnReuseTimeout [
	| client firstSocket secondSocket|
	client := ZnClient new.
	client connectionReuseTimeout: 0.
	self withServerDo: [ :server | 
		client 
			url: server localUrl;
			get.
		firstSocket := client connection.
		client get.
		secondSocket := client connection.		
		self assert: (firstSocket = secondSocket) not.
		self deny: firstSocket isConnected.
		firstSocket close.
		client close ]
]

{ #category : #testing }
ZnClientTest >> testConstruction [
	| client |
	(client := ZnClient new)
		http;
		host: 'www.example.com';
		port: 8080;
		addPath: 'foo';
		addPath: 'a space';
		addPath: 'file.txt';
		queryAt: 'key' put: '123456';
		headerAt: 'X-token' put: 'ABCDEF'.
	self assert: client request url host equals: 'www.example.com'.
	self assert: client request url port equals: 8080.
	self assert: client request url pathQueryFragmentPrintString equals: '/foo/a%20space/file.txt?key=123456'.
	self assert: (client request headers at: 'X-token') equals: 'ABCDEF'
]

{ #category : #testing }
ZnClientTest >> testCookies [
	| client |
	self
		withServerDo: [ :server | 
			server
				onRequestRespond: [ :request | 
					| cookie1 cookie2 response |
					cookie1 := request cookies detect: [ :each | each name = 'x' ] ifNone: [ ZnCookie name: 'x' value: '0' ].
					cookie2 := request cookies detect: [ :each | each name = 'y' ] ifNone: [ ZnCookie name: 'y' value: '100' ].
					cookie1 value: (cookie1 value asInteger + 1) asString.
					cookie2 value: (cookie2 value asInteger + 2) asString.
					response := ZnResponse ok: (ZnEntity text: 'OK').
					response
						addCookie: cookie1;
						addCookie: cookie2.
					response ].
			client := ZnClient new
				url: server localUrl;
				yourself.
			self assert: (client session cookieJar cookieAt: 'x' forUrl: client request url) isNil.
			self assert: (client session cookieJar cookieAt: 'y' forUrl: client request url) isNil.
			client get.
			self assert: client isSuccess.
			self assert: (client session cookieJar cookieAt: 'x' forUrl: client request url) value equals: '1'.
			self assert: (client session cookieJar cookieAt: 'y' forUrl: client request url) value equals: '102'.
			client get.
			self assert: client isSuccess.
			self assert: (client session cookieJar cookieAt: 'x' forUrl: client request url) value equals: '2'.
			self assert: (client session cookieJar cookieAt: 'y' forUrl: client request url) value equals: '104'.
			client close ]
]

{ #category : #testing }
ZnClientTest >> testDownloadSmallHTML [
	| client |
	ZnFileSystemUtils deleteIfExists: self smallHtmlUrl pathSegments last.
	"First download to a directory"
	(client := ZnClient new)
		url: self smallHtmlUrl;
		downloadTo: ZnFileSystemUtils defaultDirectoryPath.
	self assert: client isSuccess.
	self assert: client response contentType equals: ZnMimeType textHtml.
	ZnFileSystemUtils oldFileNamed: self smallHtmlUrl pathSegments last do: [ :stream | self assert: (stream upToEnd includesSubstring: 'Small') ].
	ZnFileSystemUtils deleteIfExists: self smallHtmlUrl pathSegments last.
	"Second download to an explicitly named file"
	client
		url: self smallHtmlUrl;
		downloadTo: self smallHtmlUrl pathSegments last.
	self assert: client isSuccess.
	self assert: client response contentType equals: ZnMimeType textHtml.
	ZnFileSystemUtils oldFileNamed: self smallHtmlUrl pathSegments last do: [ :stream | self assert: (stream upToEnd includesSubstring: 'Small') ].
	client close.
	ZnFileSystemUtils deleteIfExists: self smallHtmlUrl pathSegments last
]

{ #category : #testing }
ZnClientTest >> testGetAfterPost [
	self
		withServerDo: [ :server | 
			| client |
			server
				onRequestRespond: [ :request | 
					request uri firstPathSegment = 'one'
						ifTrue:
							[ (request method = #POST and: [ request hasEntity ]) ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK for one') ] ifFalse: [ ZnResponse badRequest: request ] ]
						ifFalse: [ request uri firstPathSegment = 'two'
								ifTrue:
									[ (request method = #GET and: [ request hasEntity not ]) ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK for two') ] ifFalse: [ ZnResponse badRequest: request ] ]
								ifFalse: [ ZnResponse notFound: request uri ] ] ].
			(client := ZnClient new)
				autoResetEntityMethods: #(HEAD DELETE GET);
				url: server localUrl;
				url: 'one';
				entity: (ZnEntity text: 'One two three');
				post.
			self assert: client isSuccess.
			self assert: client contents equals: 'OK for one'.
			client
				url: 'two';
				get.
			self assert: client isSuccess.
			self assert: client contents equals: 'OK for two'.
			client close ]
]

{ #category : #testing }
ZnClientTest >> testGetForm [
	| client |
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			formAt: 'username' put: 'john';
			formAdd: 'password' -> 'secret';
			get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'john').
		self assert: (client contents includesSubstring: 'secret') ]
]

{ #category : #testing }
ZnClientTest >> testGetGeoIP [
	| result client |
	result := (client := ZnClient new)
		systemPolicy;
		url: self t3EasyGeoIPUrl;
		queryAt: 'address' put: '81.83.7.35';
		accept: ZnMimeType applicationJson;
		contentReader:
				[ :entity | testingEnvironment at: #NeoJSONReader ifPresent: [ :parserClass | parserClass fromString: entity contents ] ifAbsent: [ ^ self ] ];
		ifFail: [ ^ self fail ];
		get.
	self assert: result isDictionary.
	self assert: (result at: #country) equals: 'BE'.
	client close
]

{ #category : #testing }
ZnClientTest >> testGetLargeWideStringHTML [
	| client text |
	text := '<html>{1}<h1>Czech in Czech is {2}e{3}tina.</h1>{1}</html>' 
		format: { 
			String new: ZnUtils streamingBufferSize withAll: $X.
			269 asCharacter. 
			353 asCharacter }.
	self assert: text isWideString.
	self withServerDo: [ :server |
		server onRequestRespond: [ :request | ZnResponse ok: (ZnEntity html: text) ].
		(client := ZnClient new)
			get: server localUrl.
		self assert: client isSuccess.
		self assert: client response contents equals: text.
		client close ]
]

{ #category : #testing }
ZnClientTest >> testGetSmallHTML [
	| client |
	(client := ZnClient new)
		url: self smallHtmlUrl;
		get.
	self assert: client isSuccess.
	self assert: client isContentTypeAcceptable.
	self assert: client response contentType equals: ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	self assert: client isConnected.
	client close.
	self deny: client isConnected
]

{ #category : #testing }
ZnClientTest >> testGetSmallHTMLBinary [
	| client html bytes |
	(client := ZnClient new)
		url: self smallHtmlUrl;
		get.
	self assert: client isSuccess.
	self assert: client response contentType equals: ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	html := client contents.
	self assert: html isString.

	client
		beBinary;
		get.
	self assert: client isSuccess.
	self assert: client response contentType equals: ZnMimeType textHtml.
	bytes := client contents.
	self deny: bytes isString.
	self assert: bytes utf8Decoded equals: html.

	client close
]

{ #category : #testing }
ZnClientTest >> testGetSmallHTMLOneShot [
	| client |
	(client := ZnClient new)
		beOneShot;
		url: self smallHtmlUrl;
		get.
	self assert: client isSuccess.
	self assert: client response contentType equals: ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	self deny: client isConnected
]

{ #category : #testing }
ZnClientTest >> testGetSmallHTMLStreaming [
	| client result contents |
	result := (client := ZnClient new)
		url: self smallHtmlUrl;
		streaming: true;
		get.
	self assert: client isSuccess.
	self assert: client response contentType equals: ZnMimeType textHtml.
	self assert: result isStream.
	self assert: client entity stream equals: result.
	contents := ZnUTF8Encoder new decodeBytes: result upToEnd.
	self assert: (contents includesSubstring: 'Small').
	client close
]

{ #category : #testing }
ZnClientTest >> testGetSmallHTMLTwice [
	| client |
	(client := ZnClient new)
		url: self smallHtmlUrl;
		get.
	self assert: client isSuccess.
	self assert: client response contentType equals: ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	client
		url: self smallHtmlUrl pathPrintString;
		get.
	self assert: client isSuccess.
	self assert: client response contentType equals: ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	client close
]

{ #category : #testing }
ZnClientTest >> testGetSmallHTMLUrlConstruction [
	| client |
	(client := ZnClient new)
		http;
		host: self smallHtmlUrl host;
		path: self smallHtmlUrl pathPrintString;
		get.
	self assert: client isSuccess.
	self assert: client response contentType equals: ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	client close
]

{ #category : #testing }
ZnClientTest >> testGetWideStringHTML [
	| client text |
	text := '<html><h1>Czech is in Czech {1}e{2}tina.</h1></html>' format: { 269 asCharacter. 353 asCharacter}.
	self assert: text isWideString.
	self withServerDo: [ :server |
		server onRequestRespond: [ :request | ZnResponse ok: (ZnEntity html: text) ].
		(client := ZnClient new)
			get: server localUrl.
		self assert: client isSuccess.
		self assert: client response contents equals: text.
		client close ]
]

{ #category : #testing }
ZnClientTest >> testHeadSmallHTML [
	| client |
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			head.
		self assert: client isSuccess.
		self assert: client response contentLength > 0.
		self assert: client response hasEntity not.
		client close ]
]

{ #category : #testing }
ZnClientTest >> testIfFailNonExistingHost [
	| client |
	"Note that when there is an HTTP proxy set, there will be no direct Error"
	(client := ZnClient new)
		beOneShot;
		url: 'http://unknown-stfx.eu';
		ifFail: [ : exception |
			^ self assert: (exception isKindOf: NetworkError) | (exception isKindOf: ZnHttpUnsuccessful) ];
		get.
	self assert: client isSuccess not.
	client
		enforceHttpSuccess: true;
		get.
	self fail
]

{ #category : #testing }
ZnClientTest >> testIfFailNotFound [
	| client |
	(client := ZnClient new)
		beOneShot;
		url: 'http://stfx.eu/non-existing';
		ifFail: [ :exception |
			^ self assert: (exception isKindOf: ZnHttpUnsuccessful) ];
		get.
	self assert: client isSuccess not.
	client
		enforceHttpSuccess: true;
		get.
	self fail
]

{ #category : #testing }
ZnClientTest >> testIfFailWrongType [
	| client |
	(client := ZnClient new)
		beOneShot;
		accept: ZnMimeType imagePng;
		enforceAcceptContentType: true;
		url: self smallHtmlUrl;
		ifFail: [ :exception |
			^ self assert: (exception isKindOf: ZnUnexpectedContentType) ];
		get.
	self fail
]

{ #category : #testing }
ZnClientTest >> testIfModifiedSinceNotModified [
	| client  response |
	(client := ZnClient new) host: 'stfx.eu'.
	[
		response := client 
			url: '/small.html'; 
			setIfModifiedSince: (Date year: 2012 month: 9 day: 1);
			get; 
			response.
		self assert: response isNotModified.
		self assert: response hasEntity not ] ensure: [ client close ]
]

{ #category : #testing }
ZnClientTest >> testLogging [
	| client logEventCollection count |
	logEventCollection := OrderedCollection new.
	ZnLogEvent announcer 
		when: ZnLogEvent do: [ :event | logEventCollection add: event ]. 
	(client := ZnClient new)
		clientId: #C1;
		beOneShot.
	client get: self smallHtmlUrl.
	client close.
	count := ZnLogEvent announcer numberOfSubscriptions.
	ZnLogEvent announcer 
		unsubscribe: self.
	self assert: ZnLogEvent announcer numberOfSubscriptions equals: count -1.
	self deny: logEventCollection isEmpty.
	self assert: logEventCollection anyOne clientId equals: #C1
]

{ #category : #testing }
ZnClientTest >> testOptions [
	| client |
	self withServerDo: [ :server |
		server onRequestRespond: [ :request | 
			request method = #OPTIONS
				ifTrue: [ | response |
					response := ZnResponse noContent.
					response headers at: #Allow put: 'GET, HEAD'.
					response ]
				ifFalse: [ ZnResponse badRequest: request ] ].
		(client := ZnClient new)
			options: server localUrl.
		self assert: client isSuccess.
		self deny: client response hasEntity.
		self assert: (client response headers at: #Allow) equals: 'GET, HEAD'.
		client close ]
]

{ #category : #testing }
ZnClientTest >> testPatch [
	| client |
	self withServerDo: [ :server |
		server onRequestRespond: [ :request | 
			request method = #PATCH
				ifTrue: [| response |
					response := ZnResponse noContent.
					response headers at: #Etag put: '"e0023aa4f"'.
					response]
				ifFalse: [ ZnResponse badRequest: request ] ].
		(client := ZnClient new)
			patch: server localUrl contents: 'Some data as text'.
		self assert: client isSuccess.
		self deny: client response hasEntity.
		self assert: (client response headers at: #Etag) equals: '"e0023aa4f"'.
		client close ]
]

{ #category : #testing }
ZnClientTest >> testPostForm [
	| client |
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			formAt: 'username' put: 'john';
			formAdd: 'password' -> 'secret';
			post.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'john').
		self assert: (client contents includesSubstring: 'secret') ]
]

{ #category : #testing }
ZnClientTest >> testPostMultipart [
	| client filename |
	ZnFileSystemUtils deleteIfExists: 'testPostMultiPart.txt'.
	filename := ZnFileSystemUtils fullNameFor: 'testPostMultiPart.txt'.
	ZnFileSystemUtils newFileNamed: filename do: [ :stream | stream nextPutAll: '0123456789' ].
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			addPart: (ZnMimePart fieldName: 'username' value: 'john');
			addPart: (ZnMimePart fieldName: 'foo' entity: (ZnEntity text: '123'));
			addPart: (ZnMimePart fieldName: 'file' fileNamed: filename);
			post.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'multipart') ].
	ZnFileSystemUtils deleteIfExists: 'testPostMultiPart.txt'.

]

{ #category : #testing }
ZnClientTest >> testPostTwice [
	| client |
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			contents: 'Foo bar!';
			post.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Foo bar!').
		client
			contents: 'Another one ?';
			post.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Another one ?').
		client close ]
]

{ #category : #testing }
ZnClientTest >> testPrepareRequest [
	| client |
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			prepareRequest: [ :request | request setAuthorization: 'my-signature' ];
			get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'my-signature').
		client
			url: server localUrl;
			addPath: 'echo';
			get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'my-signature').
		client close ]
]

{ #category : #testing }
ZnClientTest >> testProgress [
	"[ :bar |
		bar title: 'Downloading Sources...'.
		[
			ZnClient new 
				url: 'http://files.pharo.org/sources/PharoV30.sources'; 
				signalProgress: true;	
				downloadTo: FileLocator temp ]
			on: HTTPProgress 
			do: [ :progress |
				progress isEmpty ifFalse: [ bar current: progress percentage ]. 
				progress resume ] ] asJob run."
		
	self 
		should: [ 
			ZnClient new
				beOneShot;
				signalProgress: true;
				get: self smallHtmlUrl ] 
		raise: HTTPProgress

]

{ #category : #testing }
ZnClientTest >> testProgressNoIfFail [
	self 
		should: [ 
			ZnClient new
				beOneShot;
				signalProgress: true;
				ifFail: [ self fail ];
				get: self smallHtmlUrl ] 
		raise: HTTPProgress

]

{ #category : #testing }
ZnClientTest >> testQueryGoogle [
	| client |
	(client := ZnClient new)
		http;
		host: 'www.google.com';
		addPath: 'search';
		queryAt: 'q' put: 'Pharo Smalltalk';
		get.
	self assert: client isSuccess.
	self assert: (client response contentType matches: ZnMimeType textHtml).
	self assert: (client contents includesSubstring: 'pharo.org').
	client close
]

{ #category : #testing }
ZnClientTest >> testRedirect [
	| client response target |
	target := 'http://zn.stfx.eu'.
	client := ZnClient new url: target.
	client get.
	self assert: client isSuccess.
	client
		close;
		maxNumberOfRedirects: 0;
		url: target.
	self should: [ client get ] raise: ZnTooManyRedirects.
	client close.
	response := [ 
		ZnClient new
			beOneShot;
			maxNumberOfRedirects: 0;
			get: target;
			response ] on: ZnTooManyRedirects do: [ :exception | exception resume ].
	self assert: response isRedirect
]

{ #category : #testing }
ZnClientTest >> testRedirectDontFollow [
	| client target |
	target := 'http://zn.stfx.eu'.
	(client := ZnClient new) 
		dontFollowRedirects;
		get: target.
	self assert: client response isRedirect.
	client
		enforceHttpSuccess: true;
		get: target.
	self assert: client response isRedirect.
	client close
]

{ #category : #testing }
ZnClientTest >> testRedirectWithCookies [
	self
		withServerDo: [ :server | 
			| client cookie |
			server
				onRequestRespond: [ :request | 
					request uri firstPathSegment = 'one'
						ifTrue: [ (ZnResponse redirect: 'two')
								addCookie: (ZnCookie name: 'session' value: '123456');
								yourself ]
						ifFalse: [ cookie := request cookies detect: [ :each | each name = 'session' ] ifNone: [ nil ].
							(request uri firstPathSegment = 'two' and: [ cookie notNil and: [ cookie value = '123456' ] ])
								ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK!') ]
								ifFalse: [ ZnResponse badRequest: request ] ] ].
			(client := ZnClient new)
				url: server localUrl;
				addPath: 'one';
				post.
			self assert: client isSuccess.
			self assert: client contents equals: 'OK!'.
			client close ]
]

{ #category : #testing }
ZnClientTest >> testRedirectWithCustomHeader [
	self
		withServerDo: [ :server | 
			| client cookie |
			server
				onRequestRespond: [ :request | 
					(request headers at: 'X-Custom' ifAbsent: [ nil ]) = 'The Secret'
						ifFalse: [ ZnResponse badRequest: request ]
						ifTrue: [ request uri firstPathSegment = 'one'
								ifTrue: [ ZnResponse redirect: 'two' ]
								ifFalse: [ request uri firstPathSegment = 'two' ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK!') ] ifFalse: [ ZnResponse badRequest: request ] ] ] ].
			(client := ZnClient new)
				url: server localUrl;
				addPath: 'one';
				headerAt: 'X-Custom' put: 'The Secret';
				get.
			self assert: client isSuccess.
			self assert: client contents equals: 'OK!'.
			client close ]
]

{ #category : #testing }
ZnClientTest >> testRelativeRedirect [
	self
		withServerDo: [ :server | 
			| client |
			server
				onRequestRespond: [ :request | 
					request uri firstPathSegment = 'one'
						ifTrue: [ ZnResponse redirect: 'two' ]
						ifFalse: [ request uri firstPathSegment = 'two' ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK!') ] ifFalse: [ ZnResponse notFound: request uri ] ] ].
			(client := ZnClient new)
				url: server localUrl;
				addPath: 'one';
				post.
			self assert: client isSuccess.
			self assert: client contents equals: 'OK!'.
			client close ]
]

{ #category : #testing }
ZnClientTest >> testRelativeRedirect307 [
	self
		withServerDo: [ :server | 
			| client |
			server
				onRequestRespond: [ :request | 
					request uri firstPathSegment = 'one'
						ifTrue: [ (ZnResponse redirect: 'two')
								statusLine: (ZnStatusLine code: 307);
								yourself ]
						ifFalse: [ (request uri firstPathSegment = 'two' and: [ request method = #POST and: [ request contents = 'BODY' ] ])
								ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK!') ]
								ifFalse: [ ZnResponse notFound: request uri ] ] ].
			(client := ZnClient new)
				url: server localUrl;
				addPath: 'one';
				entity: (ZnEntity text: 'BODY');
				post.
			self assert: client isSuccess.
			self assert: client contents equals: 'OK!'.
			client close ]
]

{ #category : #testing }
ZnClientTest >> testSpecialPosts [
	self skip.
	self withServerDo: [ :server |
		{ ZnEntity with: 'A'. ZnEntity with: #[ 42 ]. ZnEntity with: ''. ZnEntity with: #[ ] } 
			do: [ :each |
				| client |
				(client := ZnClient new)
					url: server localUrl;
					addPath: #echo;
					entity: each;
					post.
				self assert: client isSuccess.
				self assert: (each isEmpty or: [client contents includesSubstring: each printString ]).
				client close.
				(client := ZnClient new)
					beOneShot;
					url: server localUrl;
					addPath: #echo;
					entity: each;
					post.
				self assert: client isSuccess.
				self assert: (each isEmpty or: [client contents includesSubstring: each printString ]) ] ]
]

{ #category : #testing }
ZnClientTest >> testTimeout [
	self withServerDo: [ :server |
		self 
			should: [ 
				ZnClient new 
					timeout: 1; 
					url: server localUrl;
					addPath: 'echo';
					queryAt: 'delay' put: '2'; 
					get ]
			raise: ConnectionTimedOut ]
]

{ #category : #testing }
ZnClientTest >> testUploadSmallDocument [
	self
		withServerDo: [ :server | 
			| client contents path |
			path := ZnFileSystemUtils fullNameFor: 'small.txt'.
			contents := String
				streamContents: [ :stream | 
					stream
						print: DateAndTime now;
						space;
						print: 9999 atRandom ].
			ZnFileSystemUtils deleteIfExists: 'small.txt'.
			ZnFileSystemUtils newFileNamed: path do: [ :stream | stream nextPutAll: contents ].
			(client := ZnClient new)
				url: server localUrl;
				addPath: 'echo';
				uploadEntityFrom: path.
			self assert: client request entity contentType equals: ZnMimeType textPlain.
			client contentType: ZnMimeType textPlain.	"Not needed, just test the code path"
			self assert: client request entity contentType equals: ZnMimeType textPlain.
			client post.
			self assert: client isSuccess.
			self assert: client entity contentType equals: ZnMimeType textPlain.
			self assert: (client contents includesSubstring: contents).
			client close.
			ZnFileSystemUtils deleteIfExists: 'small.txt' ]
]

{ #category : #testing }
ZnClientTest >> testUsernamePassword [
	self withServerDo: [ :server | | client |
		server authenticator: (ZnBasicAuthenticator username: 'foo' password: 'secret').
		(client := ZnClient new) 
			url: server localUrl;
			addPathSegment: 'echo';
			get.
		self deny: client isSuccess.
		self assert: client response isAuthenticationRequired.
		client 
			username: 'foo' password: 'secret'; 
			get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Zinc').
		client close ]
]

{ #category : #testing }
ZnClientTest >> testUsernamePasswordInUserInfo [
	self withServerDo: [ :server | | url client |
		server authenticator: (ZnBasicAuthenticator username: 'foo' password: 'secret').
		url := server localUrl.
		url username: 'foo'; password: 'secret'; addPathSegment: 'echo'.
		(client := ZnClient new) 
			get: url.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Zinc').
		client close ]
]

{ #category : #testing }
ZnClientTest >> testUsernamePasswordTwice [
	self withServerDo: [ :server | | client |
		server authenticator: (ZnBasicAuthenticator username: 'foo' password: 'secret').
		(client := ZnClient new) 
			url: server localUrl;
			addPathSegment: 'echo';
			username: 'foo' password: 'secret'; 
			get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Zinc').
		client get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Zinc').
		client close ]
]

{ #category : #private }
ZnClientTest >> withServerDo: block [
	| server |
	server := ZnServer on: 1700 + 32 atRandom.
	[ 
		server start.
		self 
			assert: server isRunning & server isListening
			description: ('Failed to start server on port {1}. Is there one already?' format: { server port }).
		block cull: server
	] 
		ensure: [ server stop ]
]
