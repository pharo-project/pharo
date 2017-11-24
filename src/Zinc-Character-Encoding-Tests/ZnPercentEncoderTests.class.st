Class {
	#name : #ZnPercentEncoderTests,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #testing }
ZnPercentEncoderTests >> testDecodePlusAsSpace [
	| encoder |
	encoder := ZnPercentEncoder new.
	self assert: (encoder decode: '+') equals: ' '.
	self assert: encoder decodePlusAsSpace.
	encoder decodePlusAsSpace: false.
	self assert: (encoder decode: '+') equals: '+'.
	self deny: encoder decodePlusAsSpace
]

{ #category : #testing }
ZnPercentEncoderTests >> testDecodingErrors [
	| encoder |
	encoder := ZnPercentEncoder new.
	self should: [ encoder decode: 'foo%%bar' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'fooçbar' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'foo%' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '%XX' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'foo%F' ] raise: ZnCharacterEncodingError
]

{ #category : #testing }
ZnPercentEncoderTests >> testLeadingZero [
	| encoder |
	encoder := ZnPercentEncoder new.
	self assert: (encoder encode: 'foo', Character tab asString, 'bar') equals: 'foo%09bar'.
	self assert: (encoder decode: 'foo%09bar') equals: 'foo', Character tab asString, 'bar'.
	self assert: (encoder encode: 'foo', Character lf asString, 'bar') equals: 'foo%0Abar'.
	self assert: (encoder decode: 'foo%0Abar') equals: 'foo', Character lf asString, 'bar'
]

{ #category : #testing }
ZnPercentEncoderTests >> testNonAscii [
	| encoder |
	encoder := ZnPercentEncoder new.
	self 
		assert: encoder characterEncoder 
		equals: (ZnCharacterEncoder newForEncoding: 'utf-8').
	self 
		assert: (encoder encode: 'élève en Français') 
		equals: '%C3%A9l%C3%A8ve%20en%20Fran%C3%A7ais'.
	self 
		assert: (encoder decode: '%C3%A9l%C3%A8ve%20en%20Fran%C3%A7ais') 
		equals: 'élève en Français'
]

{ #category : #testing }
ZnPercentEncoderTests >> testSimple [
	| encoder |
	encoder := ZnPercentEncoder new.
	self assert: (encoder encode: 'foo bar') equals: 'foo%20bar'.
	self assert: (encoder decode: 'foo%20bar') equals: 'foo bar'.
	self assert: (encoder encode: '') equals: ''.
	self assert: (encoder decode: '') equals: ''.
	self assert: (encoder decode: 'foo%25bar') equals: 'foo%bar'.
	self assert: (encoder decode: 'foo+bar') equals: 'foo bar'
]

{ #category : #testing }
ZnPercentEncoderTests >> testStringUrlDecoded [
	self assert: ('foo%20bar' urlDecoded) equals: 'foo bar' 
]

{ #category : #testing }
ZnPercentEncoderTests >> testStringUrlEncoded [
	self assert: ('foo bar' urlEncoded) equals: 'foo%20bar'
]
