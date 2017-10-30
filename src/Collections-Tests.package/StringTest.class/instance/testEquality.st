tests
testEquality
	"self run: #testEquality"
	
	self assert: 'abc' = 'abc' asWideString.
	self assert: 'abc' asWideString = 'abc'.
	self assert: ( #[97 0 0 0] asString ~= 'a000' asWideString).
	self assert: ('a000' asWideString ~=  #[97 0 0 0] asString).

	self assert: ('abc' sameAs: 'aBc' asWideString).
	self assert: ('aBc' asWideString sameAs: 'abc').
	self assert: ( #[97 0 0 0] asString 
						sameAs: 'Abcd' asWideString) not.
	self assert: ('a000' asWideString sameAs: #[97 0 0 0] asString) not.