"
Test for user level Base64 access
"
Class {
	#name : #Base64Test,
	#superclass : #TestCase,
	#category : #'Network-Tests-Kernel'
}

{ #category : #tests }
Base64Test >> testHelloWorldEncoding [
	| bytes base64Encoded |
	bytes := 'Hello World!' utf8Encoded.
	base64Encoded := bytes base64Encoded.
	self assert: base64Encoded base64Decoded equals: bytes.
	self assert: base64Encoded base64Decoded utf8Decoded equals: 'Hello World!'
]

{ #category : #tests }
Base64Test >> testPrimesEncoding [
	| bytes base64Encoded |
	bytes := (Integer primesUpTo: 255) asByteArray.
	base64Encoded := bytes base64Encoded.
	self assert: base64Encoded base64Decoded equals: bytes
]

{ #category : #tests }
Base64Test >> testRangeEncoding [
	| bytes base64Encoded |
	bytes := (0 to: 255) asByteArray.
	base64Encoded := bytes base64Encoded.
	self assert: base64Encoded base64Decoded equals: bytes
]

{ #category : #tests }
Base64Test >> testWikipediaExampleQuote [
	"https://en.wikipedia.org/wiki/Base64#Examples"
	
	| in out outAsOneLine |
	in := 'Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.'.
	out := 'TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz
IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg
dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu
dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo
ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4='.
	outAsOneLine := '' join: out lines.
	self assert: (ZnNullEncoder new encodeString: in) base64Encoded equals: outAsOneLine.
	self assert: in asByteArray base64Encoded equals: outAsOneLine.
	self assert: (ZnNullEncoder new decodeBytes: out base64Decoded) equals: in.
	self assert: out base64Decoded asString equals: in
]
