Class {
	#name : #ZnRequestLineTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnRequestLineTest >> testReading [
	| requestLine string |
	string := 'GET /foo/bar/xyz.txt HTTP/1.1' , String crlf.
	requestLine := ZnRequestLine readFrom: string readStream.
	self assert: requestLine method equals: #GET.
	self assert: requestLine uriPathQueryFragment equals: '/foo/bar/xyz.txt'.
	self assert: requestLine version equals: ZnConstants defaultHTTPVersion
]

{ #category : #testing }
ZnRequestLineTest >> testReadingWrongMethod [
	| string |
	string := 'GETT /foo/bar/xyz.txt HTTP/1.1', String crlf.
	self should: [ ZnRequestLine readFrom: string readStream ] raise: ZnUnknownHttpMethod
]

{ #category : #testing }
ZnRequestLineTest >> testReadingWrongVersion [
	| string |
	string := 'GET /foo/bar/xyz.txt HTTP/1.5', String crlf.
	self should: [ ZnRequestLine readFrom: string readStream ] raise: ZnUnknownHttpVersion
]

{ #category : #testing }
ZnRequestLineTest >> testWriting [
	| requestLine string |
	requestLine := ZnRequestLine method: 'GET' uri: '/foo/bar/xyz.txt'.
	string := String streamContents: [ :stream | requestLine writeOn: stream ].
	self assert: string equals: 'GET /foo/bar/xyz.txt HTTP/1.1' , String crlf
]
