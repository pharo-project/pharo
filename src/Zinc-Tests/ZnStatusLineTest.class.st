Class {
	#name : #ZnStatusLineTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnStatusLineTest >> testInitialization [
	"Make sure everything is properly initialized"
	
	ZnConstants officialHTTPStatusCodesSpec pairsDo: [ :code :reason |
		| statusLine |
		statusLine := ZnStatusLine code: code.
		self assert: statusLine code equals: code.
		self assert: statusLine reason equals: reason ]
]

{ #category : #testing }
ZnStatusLineTest >> testReading [
	| statusLine string |
	string := 'HTTP/1.1 200 OK' , String crlf.
	statusLine := ZnStatusLine readFrom: string readStream.
	self assert: statusLine code equals: 200.
	self assert: statusLine reason equals: 'OK'.
	self assert: statusLine version equals: ZnConstants defaultHTTPVersion
]

{ #category : #testing }
ZnStatusLineTest >> testReadingNonStandardCode [
	| string statusLine |
	"There exist non-standard codes out there, like those from cloudflare"
	string := 'HTTP/1.1 525 SSL Handshake Failed', String crlf.
	statusLine := ZnStatusLine readFrom: string readStream.
	self assert: statusLine code equals: 525.
	self assert: statusLine reason equals: 'SSL Handshake Failed'
]

{ #category : #testing }
ZnStatusLineTest >> testReadingWrongCode [
	| string |
	string := 'HTTP/1.0 1000 OK', String crlf.
	self should: [ ZnStatusLine readFrom: string readStream ] raise: ZnUnknownHttpStatusCode 
]

{ #category : #testing }
ZnStatusLineTest >> testReadingWrongVersion [
	| string |
	string := 'HTTP/0.9 200 OK', String crlf.
	self should: [ ZnStatusLine readFrom: string readStream ] raise: ZnUnknownHttpVersion
]

{ #category : #testing }
ZnStatusLineTest >> testReadingWrongVersion2 [
	| string |
	string := 'HTTP/0.9 Foo OK', String crlf.
	self should: [ ZnStatusLine readFrom: string readStream ] raise: ZnUnknownHttpVersion
]

{ #category : #testing }
ZnStatusLineTest >> testWriting [
	| statusLine string |
	statusLine := ZnStatusLine ok.
	string := String streamContents: [ :stream | statusLine writeOn: stream ].
	self assert: string equals: 'HTTP/1.1 200 OK' , String crlf
]
