Class {
	#name : #ZnMimeTypeTests,
	#superclass : #TestCase,
	#category : #Zinc-Resource-Meta-Tests
}

{ #category : #accessing }
ZnMimeTypeTests >> someNonBinaryMimeTypes [
	^ #(
		'text/plain;charset=ascii' 
		'application/json;charset=utf-8' 
		'application/ston'
		'application/vnd.com.runkeeper.user+json;charset=ISO-8859-1' 
		'application/xml-dtd' 
		'application/xslt+xml' 
		'image/svg+xml' 
		'application/SGML' 
		'model/x3d+xml'
		'application/csv')
]

{ #category : #testing }
ZnMimeTypeTests >> testAsMimeType [
	self assert: 'text/plain;charset=utf-8' asZnMimeType equals: ZnMimeType textPlain.
	self assert: ZnMimeType applicationOctetStream asZnMimeType equals: ZnMimeType applicationOctetStream
]

{ #category : #testing }
ZnMimeTypeTests >> testCharset [
	| mimeType |
	mimeType := ZnMimeType textPlain.
	self assert: mimeType charSet = 'utf-8'.
	mimeType charSet: 'ascii'.
	self assert: mimeType charSet = 'ascii'.
	mimeType clearCharSet.
	self assert: mimeType charSet isNil.
	mimeType setCharSetUTF8.
	self assert: mimeType charSet = 'utf-8'
]

{ #category : #testing }
ZnMimeTypeTests >> testCopying [
	| mimeType1 mimeType2 |
	mimeType1 := ZnMimeType textPlain.
	mimeType2 := ZnMimeType textPlain.
	self assert: mimeType1 = mimeType2.
	self assert: mimeType1 parameters = mimeType2 parameters.
	mimeType1 charSet: 'utf-8'.
	self assert: mimeType1 charSet = 'utf-8'.
	mimeType2 charSet: 'latin1'.
	self assert: mimeType2 charSet = 'latin1'.
	self assert: (mimeType1 matches: mimeType2).
	self deny: mimeType1 parameters = mimeType2 parameters.
	self deny: mimeType1 charSet = mimeType2 charSet.
	
]

{ #category : #testing }
ZnMimeTypeTests >> testDefault [
	self assert: ZnMimeType default = ZnMimeType applicationOctetStream
]

{ #category : #testing }
ZnMimeTypeTests >> testIdentity [
	self assert: ZnMimeType textPlain = ZnMimeType textPlain
]

{ #category : #testing }
ZnMimeTypeTests >> testIsBinary [
	self assert: ZnMimeType applicationOctetStream isBinary.
	self assert: ZnMimeType imagePng isBinary.
	self deny: ZnMimeType textPlain isBinary.
	self deny: ZnMimeType textHtml isBinary.
	self deny: ZnMimeType textJavascript isBinary.
	self deny: ZnMimeType applicationJson isBinary.
	self someNonBinaryMimeTypes do: [ :each |
		self deny: each asZnMimeType isBinary ]
]

{ #category : #testing }
ZnMimeTypeTests >> testMatches [
	#(
		( 'text/plain' 'text/*' )
		( 'text/plain' '*/*' )
		( 'text/plain;charset=utf-8' 'text/*' ) 
		( 'text/plain;charset=utf-8' 'text/plain' )
		( 'text/plain' 'text/plain;charset=utf-8' )
		( 'text/plain;charset=utf-8' 'text/plain;charset=ascii' ) ) do: [ :each |
			self deny: each first asZnMimeType = each second asZnMimeType.
			self assert: (each first asZnMimeType matches: each second asZnMimeType) ]
]

{ #category : #testing }
ZnMimeTypeTests >> testParameters [
	| mimeType |
	mimeType := ZnMimeType main: 'text' sub: 'plain'.
	self should: [ mimeType parameterAt: 'foo' ] raise: KeyNotFound.
	self assert: (mimeType parameterAt: 'foo' ifAbsent: [ #none ]) = #none.
	mimeType parameterAt: 'foo' put: '1'.
	self assert: (mimeType parameterAt: 'foo') = '1'.
	self assert: (mimeType parameterAt: 'foo' ifAbsent: [ #none ]) = '1'.
	mimeType removeParameter: 'foo'.
	mimeType removeParameter: 'bar'.
	self should: [ mimeType parameterAt: 'foo' ] raise: KeyNotFound.
	self assert: (mimeType parameterAt: 'foo' ifAbsent: [ #none ]) = #none	
]

{ #category : #testing }
ZnMimeTypeTests >> testReading [
	| mimeType |
	mimeType := ZnMimeType fromString: 'text/plain; charset=utf-8'.
	self assert: (mimeType main = 'text').
	self assert: (mimeType sub = 'plain').
	self assert: (mimeType charSet = 'utf-8').
	self assert: mimeType isCharSetUTF8.
	self assert: mimeType isBinary not
]

{ #category : #testing }
ZnMimeTypeTests >> testWriting [
	| mimeType |
	(mimeType := ZnMimeType main: 'text' sub: 'plain')
		charSet: 'utf-8'.
	self assert: mimeType printString = 'text/plain;charset=utf-8'.
	self assert: mimeType isCharSetUTF8.
	self assert: mimeType isBinary not
]
