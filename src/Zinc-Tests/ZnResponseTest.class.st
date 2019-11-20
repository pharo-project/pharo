Class {
	#name : #ZnResponseTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnResponseTest >> testAbsoluteCreated [
	| response uri |
	uri := 'http://www.example.com:8888/something/else/foo.txt'.
	response := ZnResponse created: uri asZnUrl.
	self assert: response code equals: 201.
	self assert: response hasHeaders.
	self assert: (response headers at: 'Location') equals: uri.
	self assert: response hasEntity.
	self assert: (response entity contents includesSubstring: uri)
]

{ #category : #testing }
ZnResponseTest >> testAbsoluteRedirect [
	| response uri |
	uri := 'http://foo.com:8080/something/else/foo.txt?key=123'.
	response := ZnResponse redirect: uri asZnUrl.
	self assert: response code equals: 302.
	self assert: response isRedirect.
	self assert: response hasHeaders.
	self assert: (response headers at: 'Location') equals: uri.
	self assert: response hasEntity.
	self assert: (response entity contents includesSubstring: uri)
]

{ #category : #testing }
ZnResponseTest >> testCookie [
	| response cookie1  responseString |
	response := ZnResponse ok: (ZnStringEntity html: '<h1>hello world</h1>').
	cookie1 := ZnCookie new.
	cookie1 
		name: 'testCookie1';
		value: '123'.
		
	response addCookie: cookie1.
	
	responseString := String streamContents: [ :stream | response writeOn: stream ].
	
	self assert: (responseString includesSubstring: 'Set-Cookie: testCookie1=123')	

	
		
		

]

{ #category : #testing }
ZnResponseTest >> testCookies [
	| response cookie1 cookie2 responseString |
	response := ZnResponse ok: (ZnStringEntity html: '<h1>hello world</h1>').
	cookie1 := ZnCookie new.
	cookie1 
		name: 'testCookie1';
		value: '123'.
		
	cookie2 := ZnCookie new.
	cookie2 
		name: 'testCookie2';
		value: '321'.
		
	response addCookie: cookie1.
	response addCookie: cookie2.
	
	responseString := String streamContents: [ :stream | response writeOn: stream ].
	
	self assert: (responseString includesSubstring: 'Set-Cookie: testCookie1=123').  
	self assert: (responseString includesSubstring: 'Set-Cookie: testCookie2=321').  	

	
		
		

]

{ #category : #testing }
ZnResponseTest >> testCreated [
	| response uri |
	uri := '/something/else/foo.txt'.
	response := ZnResponse created: uri.
	self assert: response code equals: 201.
	self assert: response hasHeaders.
	self assert: (response headers at: 'Location') equals: uri.
	self assert: response hasEntity.
	self assert: (response entity contents includesSubstring: uri)
]

{ #category : #testing }
ZnResponseTest >> testDefaultUTF8Encoding [
	| json entity response bytes result |
	"Even if we do not specificy UTF-8 explicitly, it should be the default"
	json := '{''bank'':''Région Genève Rhône''}'.
	entity := ZnEntity with: json type: 'application/json'.
	response := ZnResponse ok: entity.
	bytes := ByteArray streamContents: [ :out | response writeOn: out ].
	result := ZnResponse readFrom: bytes readStream.
	self assert: result entity contents equals: json
]

{ #category : #testing }
ZnResponseTest >> testIsError [
	| response |
	response := ZnResponse ok: (ZnStringEntity html: '<h1>hello world</h1>').
	self assert: response isError not.
	response := ZnResponse notFound: 'http:///secret.txt'.
	self assert: response isError.
	response := ZnResponse redirect: '/something/else/foo.txt'.
	self assert: response isError not
]

{ #category : #testing }
ZnResponseTest >> testNotFound [
	| response |
	response := ZnResponse notFound: 'http:///secret.txt'.
	self assert: response code equals: 404.
	self assert: (response headers includesKey: 'Server').
	self assert: (response headers includesKey: 'Date').
	self assert: (response entity contents includesSubstring: 'secret.txt')
]

{ #category : #testing }
ZnResponseTest >> testRedirect [
	| response uri |
	uri := '/something/else/foo.txt'.
	response := ZnResponse redirect: uri.
	self assert: response code equals: 302.
	self assert: response code equals: 302.
	self assert: response hasHeaders.
	self assert: (response headers at: 'Location') equals: uri.
	self assert: response hasEntity.
	self assert: (response entity contents includesSubstring: uri).
	response := ZnResponse redirect: uri asZnUrl.
	self assert: response code equals: 302.
	self assert: response code equals: 302.
	self assert: response hasHeaders.
	self assert: (response headers at: 'Location') equals: uri.
	self assert: response hasEntity.
	self assert: (response entity contents includesSubstring: uri)
]

{ #category : #testing }
ZnResponseTest >> testSlashdotGzipChunked [
	| request stream response contents |
	(request := ZnRequest get: 'http://slashdot.org')
		setConnectionClose;
		setAcceptEncodingGzip.
	stream := ZnNetworkingUtils socketStreamToUrl: request url.
	response := [ request writeOn: stream.
	stream flush.
	ZnDefaultCharacterEncoder value: ZnNullEncoder new during: [ ZnResponse readFrom: stream ] ]
		ensure: [ stream close ].
	response isRedirect ifTrue: [ ^ self ].
	self assert: response code equals: 200.
	self assert: ((response headers at: 'Content-Type') beginsWith: 'text/html').
	self assert: (response headers at: 'Content-Encoding' ifAbsent: [ 'gzip' ]) equals: 'gzip'.
	self assert: (response headers at: 'Transfer-Encoding' ifAbsent: [ 'chunked' ]) equals: 'chunked'.
	contents := response entity contents.
	self assert: ((contents includesSubstring: 'Slashdot') or: [ contents includesSubstring: 'refresh' ])
]

{ #category : #testing }
ZnResponseTest >> testWritingBinary [
	| response bytes string |
	response := ZnResponse ok: (ZnEntity text: 'Okay').
	bytes := ByteArray streamContents: [ :stream | response writeOn: stream ].
	string := bytes asString.
	self assert: (string includesSubstring: '200 OK').
	self assert: (string includesSubstring: 'Okay')
]

{ #category : #testing }
ZnResponseTest >> testWritingCharacter [
	| response string |
	response := ZnResponse ok: (ZnEntity text: 'Okay').
	string := String streamContents: [ :stream | response writeOn: stream ].
	self assert: (string includesSubstring: '200 OK').
	self assert: (string includesSubstring: 'Okay')
]
