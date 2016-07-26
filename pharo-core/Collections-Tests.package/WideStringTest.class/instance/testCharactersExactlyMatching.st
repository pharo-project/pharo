tests - match
testCharactersExactlyMatching
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"
	
	self assert: ('abc' charactersExactlyMatching: 'abc') = 3.
	self assert: ('abd' charactersExactlyMatching: 'abc') = 2.
	self assert: ('abc' charactersExactlyMatching: 'abc' asWideString) = 3.
	self assert: ('abd' charactersExactlyMatching: 'abc' asWideString) = 2.
	self assert: ('abc' asWideString charactersExactlyMatching: 'abc') = 3.
	self assert: ('abd' asWideString charactersExactlyMatching: 'abc') = 2.
	self assert: ('abc' asWideString charactersExactlyMatching: 'abc' asWideString) = 3.
	self assert: ('abd' asWideString charactersExactlyMatching: 'abc' asWideString)= 2.
	self assert: ('abc' charactersExactlyMatching: 'ABC') = 0.

