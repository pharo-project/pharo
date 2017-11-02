Class {
	#name : #ZnResourceMetaUtilsTests,
	#superclass : #TestCase,
	#category : 'Zinc-Resource-Meta-Tests'
}

{ #category : #testing }
ZnResourceMetaUtilsTests >> testDecodePercent [
	self assert: (ZnResourceMetaUtils decodePercent: 'foo%20bar') equals: 'foo bar'.
	self should: [ ZnResourceMetaUtils decodePercent: 'foo%%bar' ] raise: ZnCharacterEncodingError 
]

{ #category : #testing }
ZnResourceMetaUtilsTests >> testQueryParsing [
	| string fields |
	string := 'foo=100&x=', (ZnPercentEncoder new encode: '/a b').
	fields := ZnResourceMetaUtils parseQueryFrom: string readStream.
	self assert: (fields at: 'foo') = '100'.
	self assert: (fields at: 'x') = '/a b'.
	
]

{ #category : #testing }
ZnResourceMetaUtilsTests >> testQueryWriting [
	| fields string |
	(fields := Dictionary new)
		at: 'foo' put: '100';
		at: 'bar' put: 'a b'.
	string := String streamContents: [ :stream | 
		ZnResourceMetaUtils writeQueryFields: fields on: stream ].
	self assert: (string includesSubstring: 'foo=1').
	self assert: (string includesSubstring: 'bar=a%20b').
]
