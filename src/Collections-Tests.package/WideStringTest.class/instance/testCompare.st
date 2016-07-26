tests - compare
testCompare
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"
	
	self assert: ('abc' compare: 'abc') = 2.
	self assert: ('abc' compare: 'abd') = 1.
	self assert: ('abd' compare: 'abc') = 3.
	self assert: ('abc' compare: 'abC') = 2.
	self assert: ('abc' compare: 'abD') = 1.
	self assert: ('abd' compare: 'abC') = 3.
	self assert: ('aBc' compare: 'abC') = 2.
	self assert: ('aBc' compare: 'abD') = 1.
	self assert: ('aDd' compare: 'abC') = 3.
	
	
	self assert: ('abc' compare: 'abc' asWideString) = 2.
	self assert: ('abc' compare: 'abd' asWideString) = 1.
	self assert: ('abd' compare: 'abc' asWideString) = 3.
	self assert: ('abc' compare: 'abC' asWideString) = 2.
	self assert: ('abc' compare: 'abD' asWideString) = 1.
	self assert: ('abd' compare: 'abC' asWideString) = 3.
	self assert: ('aBc' compare: 'abC' asWideString) = 2.
	self assert: ('aBc' compare: 'abD' asWideString) = 1.
	self assert: ('aDd' compare: 'abC' asWideString) = 3.
	
	self assert: ('abc' asWideString compare: 'abc') = 2.
	self assert: ('abc' asWideString compare: 'abd') = 1.
	self assert: ('abd' asWideString compare: 'abc') = 3.
	self assert: ('abc' asWideString compare: 'abC') = 2.
	self assert: ('abc' asWideString compare: 'abD') = 1.
	self assert: ('abd' asWideString compare: 'abC') = 3.
	self assert: ('aBc' asWideString compare: 'abC') = 2.
	self assert: ('aBc' asWideString compare: 'abD') = 1.
	self assert: ('aDd' asWideString compare: 'abC') = 3.
	
	self assert: ('abc' asWideString compare: 'abc' asWideString) = 2.
	self assert: ('abc' asWideString compare: 'abd' asWideString) = 1.
	self assert: ('abd' asWideString compare: 'abc' asWideString) = 3.
	self assert: ('abc' asWideString compare: 'abC' asWideString) = 2.
	self assert: ('abc' asWideString compare: 'abD' asWideString) = 1.
	self assert: ('abd' asWideString compare: 'abC' asWideString) = 3.
	self assert: ('aBc' asWideString compare: 'abC' asWideString) = 2.
	self assert: ('aBc' asWideString compare: 'abD' asWideString) = 1.
	self assert: ('aDd' asWideString compare: 'abC' asWideString) = 3.
	
	self assert: ('abc' compare: 'abc' caseSensitive: true) = 2.
	self assert: ('abc' compare: 'abC' caseSensitive: false) = 2.
	self assert: ('abc' compare: 'abc' asWideString caseSensitive: true) = 2.
	self assert: ('abc' compare: 'abC' asWideString caseSensitive: false) = 2.
	self assert: ('abc' asWideString compare: 'abc' caseSensitive: true) = 2.
	self assert: ('abc' asWideString compare: 'abC' caseSensitive: false) = 2.
	self assert: ('abc' asWideString compare: 'abc' asWideString caseSensitive: true) = 2.
	self assert: ('abc' asWideString compare: 'abC' asWideString caseSensitive: false) = 2.