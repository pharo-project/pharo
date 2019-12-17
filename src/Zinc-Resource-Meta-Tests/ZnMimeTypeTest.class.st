Class {
	#name : #ZnMimeTypeTest,
	#superclass : #TestCase,
	#category : #'Zinc-Resource-Meta-Tests'
}

{ #category : #accessing }
ZnMimeTypeTest >> someNonBinaryMimeTypes [
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
ZnMimeTypeTest >> testAsMimeType [
	self assert: 'text/plain;charset=utf-8' asZnMimeType equals: ZnMimeType textPlain.
	self assert: ZnMimeType applicationOctetStream asZnMimeType equals: ZnMimeType applicationOctetStream
]

{ #category : #testing }
ZnMimeTypeTest >> testCharset [

	| mimeType |

	mimeType := ZnMimeType textPlain.
	self assert: mimeType charSet equals: 'utf-8'.
	mimeType charSet: 'ascii'.
	self assert: mimeType charSet equals: 'ascii'.
	mimeType clearCharSet.
	self assert: mimeType charSet isNil.
	mimeType setCharSetUTF8.
	self assert: mimeType charSet equals: 'utf-8'
]

{ #category : #testing }
ZnMimeTypeTest >> testComparingWithParameters [

	| mimeType equalMimeType |

	mimeType := ZnMimeType fromString: 'application/json;q=1'.
	equalMimeType := ZnMimeType applicationJson parameterAt: 'q' put: '1'.

	self
		assert: mimeType equals: equalMimeType;
		assert: mimeType hash equals: equalMimeType hash;
		deny: mimeType equals: ZnMimeType applicationJson
]

{ #category : #testing }
ZnMimeTypeTest >> testCopying [
	| mimeType1 mimeType2 |
	mimeType1 := ZnMimeType textPlain.
	mimeType2 := ZnMimeType textPlain.
	self assert: mimeType1 equals: mimeType2.
	self assert: mimeType1 parameters equals: mimeType2 parameters.
	mimeType1 charSet: 'utf-8'.
	self assert: mimeType1 charSet equals: 'utf-8'.
	mimeType2 charSet: 'latin1'.
	self assert: mimeType2 charSet equals: 'latin1'.
	self assert: (mimeType1 matches: mimeType2).
	self deny: mimeType1 parameters equals: mimeType2 parameters.
	self deny: mimeType1 charSet equals: mimeType2 charSet
]

{ #category : #testing }
ZnMimeTypeTest >> testDefault [

	self assert: ZnMimeType default equals: ZnMimeType applicationOctetStream
]

{ #category : #testing }
ZnMimeTypeTest >> testIdentity [

	self assert: ZnMimeType textPlain equals: ZnMimeType textPlain
]

{ #category : #testing }
ZnMimeTypeTest >> testIsBinary [
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
ZnMimeTypeTest >> testMatches [
	#(#('text/plain' 'text/*') #('text/plain' '*/*') #('text/plain;charset=utf-8' 'text/*') #('text/plain;charset=utf-8' 'text/plain') #('text/plain' 'text/plain;charset=utf-8') #('text/plain;charset=utf-8' 'text/plain;charset=ascii'))
		do: [ :each | 
			self deny: each first asZnMimeType equals: each second asZnMimeType.
			self assert: (each first asZnMimeType matches: each second asZnMimeType) ]
]

{ #category : #testing }
ZnMimeTypeTest >> testParameters [

	| mimeType |

	mimeType := ZnMimeType main: 'text' sub: 'plain'.
	self should: [ mimeType parameterAt: 'foo' ] raise: KeyNotFound.
	self assert: ( mimeType parameterAt: 'foo' ifAbsent: [ #none ] ) equals: #none.
	mimeType parameterAt: 'foo' put: '1'.
	self assert: ( mimeType parameterAt: 'foo' ) equals: '1'.
	self assert: ( mimeType parameterAt: 'foo' ifAbsent: [ #none ] ) equals: '1'.
	mimeType removeParameter: 'foo'.
	mimeType removeParameter: 'bar'.
	self should: [ mimeType parameterAt: 'foo' ] raise: KeyNotFound.
	self assert: ( mimeType parameterAt: 'foo' ifAbsent: [ #none ] ) equals: #none
]

{ #category : #testing }
ZnMimeTypeTest >> testReading [

	| mimeType |

	mimeType := ZnMimeType fromString: 'text/plain; charset=utf-8'.
	self assert: mimeType main equals: 'text'.
	self assert: mimeType sub equals: 'plain'.
	self assert: mimeType charSet equals: 'utf-8'.
	self assert: mimeType isCharSetUTF8.
	self assert: mimeType isBinary not
]

{ #category : #testing }
ZnMimeTypeTest >> testWriting [

	| mimeType |

	( mimeType := ZnMimeType main: 'text' sub: 'plain' ) charSet: 'utf-8'.
	self assert: mimeType printString equals: 'text/plain;charset=utf-8'.
	self assert: mimeType isCharSetUTF8.
	self assert: mimeType isBinary not
]
