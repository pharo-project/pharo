"
SUnit tests for EUC-JP encoding
"
Class {
	#name : #EUCTextConverterTest,
	#superclass : #TestCase,
	#category : #'Multilingual-Tests-OtherLanguages'
}

{ #category : #testing }
EUCTextConverterTest >> testEUCJP [
	| encodingName halfwidthKatakanaLetterKa cjkUnifiedIdeograph6CB4 cjkUnifiedIdeograph50BA encodedHalfwidthKatakanaLetterKa encodedCJKUnifiedIdeograph6CB4 encodedCJKUnifiedIdeograph50BA japaneseInJapanese |
	encodingName := 'euc-jp'.

	"Example characters taken from: http://www.sljfaq.org/afaq/encodings.html#encodings-EUC-JP"
	halfwidthKatakanaLetterKa := Character codePoint: 16rFF76.
	cjkUnifiedIdeograph6CB4 := Character codePoint: 16r6CB3.
	cjkUnifiedIdeograph50BA := Character codePoint: 16r50BA.

	encodedHalfwidthKatakanaLetterKa := #[16r8E 16rB6].
	encodedCJKUnifiedIdeograph6CB4 := #[16rB2 16rCF].
	encodedCJKUnifiedIdeograph50BA := #[16r8F 16rB2 16rB0].

	self assert: ((String with: cjkUnifiedIdeograph6CB4) convertToEncoding: encodingName) asByteArray equals: encodedCJKUnifiedIdeograph6CB4.
	"EUCJPTextConverter does not currently support encoding the characters defined in the JIS-X-0201 and JIS-X-0212 standards:"
	self assert: ((String with: halfwidthKatakanaLetterKa) convertToEncoding: encodingName) asByteArray equals: #[].
	self assert: ((String with: cjkUnifiedIdeograph50BA) convertToEncoding: encodingName) asByteArray equals: #[].

	self assert: (encodedCJKUnifiedIdeograph6CB4 asString convertFromEncoding: encodingName) equals: (String with: cjkUnifiedIdeograph6CB4).
	"EUCJPTextConverter does not currently support decoding the representations that start with the 8E and 8F markers:"
	self assert: (encodedHalfwidthKatakanaLetterKa asString convertFromEncoding: encodingName) equals: String new.
	self assert: (encodedCJKUnifiedIdeograph50BA asString convertFromEncoding: encodingName) equals: String new.


	japaneseInJapanese := #(16r65E5 16r672C 16r8A9E) collect: [ :codePoint | Character codePoint: codePoint ] as: String.
	((japaneseInJapanese convertToEncoding: encodingName) convertFromEncoding: encodingName) = japaneseInJapanese
]
