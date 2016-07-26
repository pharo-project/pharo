tests - relation order
testRelationOrderWithCase
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"
		
	self assert: ('ABC' caseInsensitiveLessOrEqual: 'abc').
	self assert: ('ABC' caseInsensitiveLessOrEqual: 'abd').
	self assert: ('ABD' caseInsensitiveLessOrEqual: 'abc') not.
	self assert: ('ABC' caseInsensitiveLessOrEqual: 'abc' asWideString).
	self assert: ('ABC' caseInsensitiveLessOrEqual: 'abd' asWideString).
	self assert: ('ABD' caseInsensitiveLessOrEqual: 'abc' asWideString) not.
	self assert: ('ABC' asWideString caseInsensitiveLessOrEqual: 'abc').
	self assert: ('ABC' asWideString caseInsensitiveLessOrEqual: 'abd').
	self assert: ('ABD' asWideString caseInsensitiveLessOrEqual: 'abc') not.
	self assert: ('ABC' asWideString caseInsensitiveLessOrEqual: 'abc' asWideString).
	self assert: ('ABC' asWideString caseInsensitiveLessOrEqual: 'abd' asWideString).
	self assert: ('ABD' asWideString caseInsensitiveLessOrEqual: 'abc' asWideString) not.
	
	
	self assert: ('abc' caseSensitiveLessOrEqual: 'abc').
	self assert: ('abc' caseSensitiveLessOrEqual: 'abd').
	self assert: ('abd' caseSensitiveLessOrEqual: 'abc') not.
	self assert: ('abc' caseSensitiveLessOrEqual: 'abc' asWideString).
	self assert: ('abc' caseSensitiveLessOrEqual: 'abd' asWideString).
	self assert: ('abd' caseSensitiveLessOrEqual: 'abc' asWideString) not.
	self assert: ('abc' asWideString caseSensitiveLessOrEqual: 'abc').
	self assert: ('abc' asWideString caseSensitiveLessOrEqual: 'abd').
	self assert: ('abd' asWideString caseSensitiveLessOrEqual: 'abc') not.
	self assert: ('abc' caseSensitiveLessOrEqual: 'ABC') not.
	