Class {
	#name : #ZnRequestLineTests,
	#superclass : #TestCase,
	#category : 'Zinc-Tests'
}

{ #category : #testing }
ZnRequestLineTests >> testReading [
	| requestLine string |
	string := 'GET /foo/bar/xyz.txt HTTP/1.1', String crlf.
	requestLine := ZnRequestLine readFrom: string readStream.
	self assert: requestLine method = #GET.
	self assert: requestLine uriPathQueryFragment = '/foo/bar/xyz.txt'.
	self assert: requestLine version = ZnConstants defaultHTTPVersion 
]

{ #category : #testing }
ZnRequestLineTests >> testReadingWrongMethod [
	| string |
	string := 'GETT /foo/bar/xyz.txt HTTP/1.1', String crlf.
	self should: [ ZnRequestLine readFrom: string readStream ] raise: ZnUnknownHttpMethod
]

{ #category : #testing }
ZnRequestLineTests >> testReadingWrongVersion [
	| string |
	string := 'GET /foo/bar/xyz.txt HTTP/1.5', String crlf.
	self should: [ ZnRequestLine readFrom: string readStream ] raise: ZnUnknownHttpVersion
]

{ #category : #testing }
ZnRequestLineTests >> testWriting [
	| requestLine string |
	requestLine := ZnRequestLine method: 'GET' uri: '/foo/bar/xyz.txt'.
	string := String streamContents: [ :stream | requestLine writeOn: stream ].
	self assert: string = ('GET /foo/bar/xyz.txt HTTP/1.1', String crlf)
]
