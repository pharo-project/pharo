comparing
compare: string1 with: string2 collated: order
	"
	self assert: 'abc' = 'abc' asWideString.
	self assert: 'abc' asWideString = 'abc'.
	self assert: ((ByteArray with: 97 with: 0 with: 0 with: 0) asString ~= 'a000' asWideString).
	self assert: ('a000' asWideString ~= (ByteArray with: 97 with: 0 with: 0 with: 0) asString).

	self assert: ('abc' sameAs: 'aBc' asWideString).
	self assert: ('aBc' asWideString sameAs: 'abc').
	self assert: ((ByteArray with: 97 with: 0 with: 0 with: 0) asString sameAs: 'Abcd' asWideString) not.
	self assert: ('a000' asWideString sameAs: (ByteArray with: 97 with: 0 with: 0 with: 0) asString) not.

	"
	(string1 isByteString and: [string2 isByteString]) ifTrue: [
		^ ByteString compare: string1 with: string2 collated: order].
     "Primitive does not fail properly right now"
	^ String compare: string1 with: string2 collated: order