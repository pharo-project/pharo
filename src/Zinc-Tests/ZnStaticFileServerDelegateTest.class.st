Class {
	#name : #ZnStaticFileServerDelegateTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #private }
ZnStaticFileServerDelegateTest >> largeHtml [
	^ String streamContents: [ :out |
		out << '<html><body>'.
		out << 'Les élèves Français'.
		(1 to: 4000) do: [ :each | out print: each ] separatedBy: [ out space ]. 
		out << '</html></body>' ]
]

{ #category : #running }
ZnStaticFileServerDelegateTest >> setUp [
	super setUp.
	ZnFileSystemUtils
		newFileNamed: 'small.html' 
		do: [ :stream |
			stream nextPutAll: self smallHtml ].
	ZnFileSystemUtils
		newFileNamed: 'large.html' 
		do: [ :stream |
			stream nextPutAll: self largeHtml ].
	ZnFileSystemUtils
		newFileNamed: 'wide.html' 
		do: [ :stream |
			stream nextPutAll: self wideHtml ].
]

{ #category : #private }
ZnStaticFileServerDelegateTest >> smallHtml [
	^ '<html>
<head><title>Small</title></head>
<body><h1>Small</h1><p>This is a small HTML document</p></body>
</html>
'
]

{ #category : #running }
ZnStaticFileServerDelegateTest >> tearDown [
	ZnFileSystemUtils 
		deleteIfExists: 'small.html'; 
		deleteIfExists: 'large.html'; 
		deleteIfExists: 'wide.html'.
	super tearDown	
	
]

{ #category : #tests }
ZnStaticFileServerDelegateTest >> testBasicGet [
	self
		withServerDo: [ :server | 
			| client |
			(client := ZnClient new)
				beOneShot;
				url: server localUrl;
				addPath: #('local-files' 'small.html');
				get.
			self assert: client isSuccess.
			self assert: client response contentType equals: ZnMimeType textHtml.
			self assert: client contents equals: self smallHtml.
			self assert: (ZnUtils parseHttpDate: (client response headers at: 'Modification-Date')) equals: (ZnFileSystemUtils modificationTimeFor: 'small.html') asUTC.
			self assert: (ZnUtils parseHttpDate: (client response headers at: 'Expires')) > (DateAndTime now + 10 days).
			self assert: (client response headers at: 'Cache-Control') equals: (server delegate maxAgeFor: ZnMimeType textHtml) ]
]

{ #category : #tests }
ZnStaticFileServerDelegateTest >> testBasicGetLarge [
	self
		withServerDo: [ :server | 
			| client |
			(client := ZnClient new)
				beOneShot;
				url: server localUrl;
				addPath: #('local-files' 'large.html');
				get.
			self assert: client isSuccess.
			self assert: client response contentType equals: ZnMimeType textHtml.
			self assert: client contents equals: self largeHtml.
			self assert: (ZnUtils parseHttpDate: (client response headers at: 'Modification-Date')) equals: (ZnFileSystemUtils modificationTimeFor: 'large.html') asUTC.
			self assert: (ZnUtils parseHttpDate: (client response headers at: 'Expires')) > (DateAndTime now + 10 days).
			self assert: (client response headers at: 'Cache-Control') equals: (server delegate maxAgeFor: ZnMimeType textHtml) ]
]

{ #category : #tests }
ZnStaticFileServerDelegateTest >> testBasicGetWide [
	self
		withServerDo: [ :server | 
			| client |
			(client := ZnClient new)
				beOneShot;
				url: server localUrl;
				addPath: #('local-files' 'wide.html');
				get.
			self assert: client isSuccess.
			self assert: client response contentType equals: ZnMimeType textHtml.
			self assert: client contents equals: self wideHtml.
			self assert: (ZnUtils parseHttpDate: (client response headers at: 'Modification-Date')) equals: (ZnFileSystemUtils modificationTimeFor: 'large.html') asUTC.
			self assert: (ZnUtils parseHttpDate: (client response headers at: 'Expires')) > (DateAndTime now + 10 days).
			self assert: (client response headers at: 'Cache-Control') equals: (server delegate maxAgeFor: ZnMimeType textHtml) ]
]

{ #category : #tests }
ZnStaticFileServerDelegateTest >> testBasicHead [
	self
		withServerDo: [ :server | 
			| client |
			(client := ZnClient new)
				beOneShot;
				url: server localUrl;
				addPath: #('local-files' 'small.html');
				head.
			self assert: client isSuccess.
			self assert: client response contentType equals: ZnMimeType textHtml.
			self deny: client response hasEntity.
			self assert: (ZnUtils parseHttpDate: (client response headers at: 'Modification-Date')) equals: (ZnFileSystemUtils modificationTimeFor: 'small.html') asUTC.
			self assert: (ZnUtils parseHttpDate: (client response headers at: 'Expires')) > (DateAndTime now + 10 days).
			self assert: (client response headers at: 'Cache-Control') equals: (server delegate maxAgeFor: ZnMimeType textHtml) ]
]

{ #category : #tests }
ZnStaticFileServerDelegateTest >> testIfModifiedSinceNotModified [
	self withServerDo: [ :server | | client |
		(client := ZnClient new) 
			beOneShot;
			url: server localUrl;
			addPath: #('local-files' 'small.html');
			"We have to cheat a little bit since our #setUp just created the file,
			it does not alter the semantics of what we try to test"
			setIfModifiedSince: (Date today addDays: 2);
			get.
		self assert: client response isNotModified.
		self deny: client response hasEntity ]
]

{ #category : #private }
ZnStaticFileServerDelegateTest >> wideHtml [
	^ '<html><h1>Czech is in Czech {1}e{2}tina.</h1></html>' format: { 269 asCharacter. 353 asCharacter}.
]

{ #category : #private }
ZnStaticFileServerDelegateTest >> withServerDo: block [
	| server staticFileServerDelegate |
	server := ZnServer on: 1700 + 32 atRandom.
	[ 
		(staticFileServerDelegate := ZnStaticFileServerDelegate new)
			prefixFromString: 'local-files'; 
			directory: ZnFileSystemUtils defaultDirectory;
			mimeTypeExpirations: ZnStaticFileServerDelegate defaultMimeTypeExpirations. 
		server delegate: staticFileServerDelegate.
		server start.
		self 
			assert: server isRunning & server isListening
			description: ('Failed to start server on port {1}. Is there one already?' format: { server port }).
		block cull: server
	] 
		ensure: [ server stop ]
]
