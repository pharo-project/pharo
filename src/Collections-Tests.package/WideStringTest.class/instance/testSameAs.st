tests - compare
testSameAs
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"

	self assert: ('abc' sameAs: 'aBc' asWideString).
	self assert: ('aBc' asWideString sameAs: 'abc').
	self assert: ( #[97 0 0 0] asString sameAs: 'Abcd' asWideString) not.
	self assert: ('a000' asWideString sameAs:  #[97 0 0 0] asString) not.
	