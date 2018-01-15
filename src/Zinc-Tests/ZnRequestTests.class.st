Class {
	#name : #ZnRequestTests,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnRequestTests >> testBasicAuthenticate [
	| request |
	request := ZnRequest new.
	request setBasicAuthenticationUsername: 'user' password: 'secret'.
	self assert: (request headers at: 'Authorization') = 'Basic dXNlcjpzZWNyZXQ='.
	self assert: request basicAuthentication asArray equals: #( 'user' 'secret' ).
	request setBasicAuthenticationUsername: 'user' password: ''.
	self assert: (request headers at: 'Authorization') = 'Basic dXNlcjo='.
	self assert: request basicAuthentication asArray equals: #( 'user' '' ).
	request setBasicAuthenticationUsername: 'user' password: ':colons:'.
	self assert: (request headers at: 'Authorization') = 'Basic dXNlcjo6Y29sb25zOg=='.
	self assert: request basicAuthentication asArray equals: #( 'user' ':colons:' ).
]

{ #category : #testing }
ZnRequestTests >> testCookiesParsing [
	| input request cookies testCookie1 testCookie2 |
	input := 'GET /foo.html HTTP/1.1', String crlf, 
		'Cookie: testCookie1=123; testCookie2=321', String crlf,
		'Host: foo.com', String crlf, 
		'Agent: SUnit', String crlf, 
		String crlf.
	request := ZnRequest readFrom: input readStream.
	
	cookies := request cookies.
	testCookie1 := cookies detect: [ :each | each name = 'testCookie1' ].
	testCookie2 := cookies detect: [ :each | each name = 'testCookie2' ].
	
	self assert: testCookie1 notNil.
	self assert: testCookie1 value = '123'.	
	
	self assert: testCookie2 notNil.
	self assert: testCookie2 value = '321'.	


]

{ #category : #testing }
ZnRequestTests >> testCookiesWriting [
	|  client testCookie1 testCookie2 cookies|
	client := ZnClient new.
	client url: 'http://www.example.com'.
	(testCookie1 := ZnCookie name: 'testCookie1' value: '123')
		domain: 'example.com'.
	(testCookie2 := ZnCookie name: 'testCookie2' value: '321')
		domain: 'example.com'.
	(client session cookieJar)
		add: testCookie1;
		add: testCookie2.
	client sendCookies.
	cookies := client request cookies do: [ :each | each defaultDomain: 'example.com' ].
	self assert: (cookies includes: testCookie1).
	self assert: (cookies includes: testCookie2)
	
		

]

{ #category : #testing }
ZnRequestTests >> testMergedFields [
	| request form mergedFields |
	request := ZnRequest post: 'http://host.com/foo?x=1&y=2&x=3'.
	form := ZnApplicationFormUrlEncodedEntity new.
	form
		at: #z put: '100';
		at: #z add: '200';
		at: #y put: '0'.
	request entity: form.
	mergedFields := request mergedFields.
	self assert: mergedFields keys sorted equals: #(x y z).
	self assert: (mergedFields at: #x) sorted equals: #('1' '3').
	self assert: (mergedFields at: #y) sorted equals: #('0' '2').
	self assert: (mergedFields at: #z) sorted equals: #('100' '200')
]

{ #category : #testing }
ZnRequestTests >> testReading [
	| input request |
	input := 'GET /foo.html HTTP/1.1', String crlf, 
		'Host: foo.com', String crlf, 
		'Agent: SUnit', String crlf, 
		String crlf.
	request := ZnRequest readFrom: input readStream.
	self assert: request method = #GET.
	self assert: request hasHeaders.
	self assert: (request headers at: 'Host') = 'foo.com'.
	self assert: request hasEntity not

]

{ #category : #testing }
ZnRequestTests >> testReadingWithEntity [
	| input request |
	input := 'GET /foo.html HTTP/1.1', String crlf, 
		'Host: foo.com', String crlf, 
		'Content-Length: 8', String crlf, 
		'Content-Type: text/plain', String crlf, 
		String crlf, 
		'12345678'.
	request := ZnRequest readFrom: input asByteArray readStream.
	self assert: request method equals: #GET.
	self assert: request hasHeaders.
	self assert: (request contentType matches: ZnMimeType textPlain).
	self assert: request contentLength equals: 8.
	self assert: request hasHeaders.
	self assert: request entity contents equals: '12345678'

]

{ #category : #testing }
ZnRequestTests >> testUrl [
	| url request input |
	url := 'http://zn.stfx.eu/zn/small.thml' asZnUrl.
	request := ZnRequest get: url.
	"The original URL is kept inside the request line object inside the request object"
	self assert: request url equals: url.
	self assert: request relativeUrl equals: url asRelativeUrl.
	self assert: request absoluteUrl equals: url.
	url := 'http://foo.com/foo.html' asZnUrl.
	input := 'GET /foo.html HTTP/1.1' , String crlf , 
		'Host: foo.com' , String crlf , 
		String crlf.
	request := ZnRequest readFrom: input readStream.
	self assert: request url equals: url asRelativeUrl.
	self assert: request relativeUrl equals: url asRelativeUrl.
	self assert: request absoluteUrl equals: url
]
