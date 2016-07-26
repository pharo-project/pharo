tests - compare
testEqual
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"
	
	self assert: 'abc' = 'abc'.
	self assert: 'abc' = 'abc' asWideString.
	self assert: 'abc' asWideString = 'abc'.
	self assert: 'abc' asWideString = 'abc' asWideString.
	self assert: ('abc' = 'ABC') not.
	self assert: ('abc' = 'ABC' asWideString) not.
	self assert: ('abc' asWideString = 'ABC') not.
	self assert: ('abc' asWideString = 'abc' asWideString).
	self assert: ( #[97 0 0 0] asString ~= 'a000' asWideString).
	self assert: ('a000' asWideString ~=  #[97 0 0 0] asString).