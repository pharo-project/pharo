"
This is the unit test for the class String. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org/)
	- the sunit class category
"
Class {
	#name : #WideStringTest,
	#superclass : #ClassTestCase,
	#category : #'Collections-Tests-Strings'
}

{ #category : #'tests - converting' }
WideStringTest >> testAsInteger [
	self assert: '1796exportFixes-tkMX' asWideString asInteger equals: 1796.
	self assert: 'donald' asWideString asInteger isNil.
	self assert: 'abc234def567' asWideString asInteger equals: 234.
	self assert: '-94' asWideString asInteger equals: -94.
	self assert: 'foo-bar-92' asWideString asInteger equals: -92.

	self assert: '1796exportFixes-tkMX' asWideString asSignedInteger equals: 1796.
	self assert: 'donald' asWideString asSignedInteger isNil.
	self assert: 'abc234def567' asWideString asSignedInteger equals: 234.
	self assert: '-94' asWideString asSignedInteger equals: -94.
	self assert: 'foo-bar-92' asWideString asSignedInteger equals: -92.

	self assert: '1796exportFixes-tkMX' asWideString asUnsignedInteger equals: 1796.
	self assert: 'donald' asWideString asUnsignedInteger isNil.
	self assert: 'abc234def567' asWideString asUnsignedInteger equals: 234.
	self assert: '-94' asWideString asUnsignedInteger equals: 94.
	self assert: 'foo-bar-92' asWideString asUnsignedInteger equals: 92
]

{ #category : #testing }
WideStringTest >> testAtPut [
	"Non regression test for http://bugs.squeak.org/view.php?id=6998"
	
	| w1 |
	w1 := WideString with: (Unicode value: 402) with: $a with: (Unicode value: 400) with: $b.
	self assert: (w1 at: 2 put: $b) = $b description: 'at:put: should return the put-object'

]

{ #category : #'tests - beginswith' }
WideStringTest >> testBeginsWith [
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"
	
	| w1 w2 |
	self assert: ('abc' beginsWith: 'ab').
	self assert: ('abc' beginsWith: 'ab' asWideString).
	self assert: ('abc' asWideString beginsWith: 'ab').
	self assert: ('abc' beginsWith: 'aX') not .
	self assert: ('abc' beginsWith: 'AB') not.
	self assert: ('abc' beginsWith: 'AB' asWideString) not .
	self assert: ('ABC' asWideString beginsWith: 'ab') not.

	w1 := WideString with: (Unicode value: 402) with: $a with: (Unicode value: 400) with: $b.
	w2 := WideString with: (Unicode value: 402).
	w1 beginsWith: w2.

]

{ #category : #'tests - match' }
WideStringTest >> testCharactersExactlyMatching [
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"

	self assert: ('abc' charactersExactlyMatching: 'abc') equals: 3.
	self assert: ('abd' charactersExactlyMatching: 'abc') equals: 2.
	self assert: ('abc' charactersExactlyMatching: 'abc' asWideString) equals: 3.
	self assert: ('abd' charactersExactlyMatching: 'abc' asWideString) equals: 2.
	self assert: ('abc' asWideString charactersExactlyMatching: 'abc') equals: 3.
	self assert: ('abd' asWideString charactersExactlyMatching: 'abc') equals: 2.
	self assert: ('abc' asWideString charactersExactlyMatching: 'abc' asWideString) equals: 3.
	self assert: ('abd' asWideString charactersExactlyMatching: 'abc' asWideString) equals: 2.
	self assert: ('abc' charactersExactlyMatching: 'ABC') equals: 0
]

{ #category : #'tests - compare' }
WideStringTest >> testCompare [
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"

	self assert: ('abc' compare: 'abc') equals: 2.
	self assert: ('abc' compare: 'abd') equals: 1.
	self assert: ('abd' compare: 'abc') equals: 3.
	self assert: ('abc' compare: 'abC') equals: 2.
	self assert: ('abc' compare: 'abD') equals: 1.
	self assert: ('abd' compare: 'abC') equals: 3.
	self assert: ('aBc' compare: 'abC') equals: 2.
	self assert: ('aBc' compare: 'abD') equals: 1.
	self assert: ('aDd' compare: 'abC') equals: 3.


	self assert: ('abc' compare: 'abc' asWideString) equals: 2.
	self assert: ('abc' compare: 'abd' asWideString) equals: 1.
	self assert: ('abd' compare: 'abc' asWideString) equals: 3.
	self assert: ('abc' compare: 'abC' asWideString) equals: 2.
	self assert: ('abc' compare: 'abD' asWideString) equals: 1.
	self assert: ('abd' compare: 'abC' asWideString) equals: 3.
	self assert: ('aBc' compare: 'abC' asWideString) equals: 2.
	self assert: ('aBc' compare: 'abD' asWideString) equals: 1.
	self assert: ('aDd' compare: 'abC' asWideString) equals: 3.

	self assert: ('abc' asWideString compare: 'abc') equals: 2.
	self assert: ('abc' asWideString compare: 'abd') equals: 1.
	self assert: ('abd' asWideString compare: 'abc') equals: 3.
	self assert: ('abc' asWideString compare: 'abC') equals: 2.
	self assert: ('abc' asWideString compare: 'abD') equals: 1.
	self assert: ('abd' asWideString compare: 'abC') equals: 3.
	self assert: ('aBc' asWideString compare: 'abC') equals: 2.
	self assert: ('aBc' asWideString compare: 'abD') equals: 1.
	self assert: ('aDd' asWideString compare: 'abC') equals: 3.

	self assert: ('abc' asWideString compare: 'abc' asWideString) equals: 2.
	self assert: ('abc' asWideString compare: 'abd' asWideString) equals: 1.
	self assert: ('abd' asWideString compare: 'abc' asWideString) equals: 3.
	self assert: ('abc' asWideString compare: 'abC' asWideString) equals: 2.
	self assert: ('abc' asWideString compare: 'abD' asWideString) equals: 1.
	self assert: ('abd' asWideString compare: 'abC' asWideString) equals: 3.
	self assert: ('aBc' asWideString compare: 'abC' asWideString) equals: 2.
	self assert: ('aBc' asWideString compare: 'abD' asWideString) equals: 1.
	self assert: ('aDd' asWideString compare: 'abC' asWideString) equals: 3.

	self assert: ('abc' compare: 'abc' caseSensitive: true) equals: 2.
	self assert: ('abc' compare: 'abC' caseSensitive: false) equals: 2.
	self assert: ('abc' compare: 'abc' asWideString caseSensitive: true) equals: 2.
	self assert: ('abc' compare: 'abC' asWideString caseSensitive: false) equals: 2.
	self assert: ('abc' asWideString compare: 'abc' caseSensitive: true) equals: 2.
	self assert: ('abc' asWideString compare: 'abC' caseSensitive: false) equals: 2.
	self assert: ('abc' asWideString compare: 'abc' asWideString caseSensitive: true) equals: 2.
	self assert: ('abc' asWideString compare: 'abC' asWideString caseSensitive: false) equals: 2
]

{ #category : #'tests - endswith' }
WideStringTest >> testEndsWith [
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"
	
	self assert: ('abc' endsWith: 'bc').
	self assert: ('abc' endsWith: 'bc' asWideString).
	self assert: ('abc' asWideString endsWith: 'bc').
	self assert: ('abc' endsWith: 'bX') not .
	self assert: ('abc' endsWith: 'BC') not.
	self assert: ('abc' endsWith: 'BC' asWideString) not .
	self assert: ('ABC' asWideString endsWith: 'bc') not.


]

{ #category : #'tests - compare' }
WideStringTest >> testEqual [
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"

	self assert: 'abc' equals: 'abc'.
	self assert: 'abc' equals: 'abc' asWideString.
	self assert: 'abc' asWideString equals: 'abc'.
	self assert: 'abc' asWideString equals: 'abc' asWideString.
	self assert: ('abc' = 'ABC') not.
	self assert: ('abc' = 'ABC' asWideString) not.
	self assert: ('abc' asWideString = 'ABC') not.
	self assert: 'abc' asWideString equals: 'abc' asWideString.
	self assert: #[97 0 0 0] asString ~= 'a000' asWideString.
	self assert: 'a000' asWideString ~= #[97 0 0 0] asString
]

{ #category : #'tests - substrings' }
WideStringTest >> testFindSubstring [
	"This is related to http://bugs.squeak.org/view.php?id=6366
	finding substring in a WideString was broken because matchTable are byte-wise"

	| ws1 ws2 |
	self assert: ('abcd' findString: 'bc' startingAt: 1) equals: 2.
	self assert: ('abcd' asWideString findString: 'bc' startingAt: 1) equals: 2.
	self assert: ('abcd' findString: 'bc' asWideString startingAt: 1) equals: 2.
	self assert: ('abcd' asWideString findString: 'bc' asWideString startingAt: 1) equals: 2.

	ws1 := 'A' , (WideString with: (Unicode value: 530)) , 'BCD'.
	self assert: (ws1 findString: 'bc' startingAt: 1 caseSensitive: true) equals: 0.
	self assert: (ws1 findString: 'bc' startingAt: 1 caseSensitive: false) equals: 3.

	ws2 := (WideString with: (Unicode value: 530)) , 'b'.
	self assert: (ws1 findString: ws2 startingAt: 1 caseSensitive: true) equals: 0.
	self assert: (ws1 findString: ws2 startingAt: 1 caseSensitive: false) equals: 2.

	self assert: ('abc' findString: ws2 startingAt: 1 caseSensitive: true) equals: 0.
	self assert: ('abc' findString: ws2 startingAt: 1 caseSensitive: false) equals: 0
]

{ #category : #testing }
WideStringTest >> testIsAsciiString [
	"Non-regression for https://pharo.manuscript.com/f/cases/15232 "

	self assert: '' asWideString isAsciiString equals: true.
	self assert: 'abcdefGHIJKL 98765,./@#%$' asWideString isAsciiString equals: true.

	self assert: 'éÀ' asWideString isAsciiString equals: false
]

{ #category : #'tests - match' }
WideStringTest >> testMatch [
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"
	
	self assert: ('*baz' match: 'mobaz' ).
	self assert: ('*foo#zort' match: 'afoo3zortthenfoo3zort' ).
	self assert: ('*baz' match: 'mobaz' ).
	self assert: ('*foo#zort' match: 'afoo3zortthenfoo3zort' ).
	
	self assert: ('*baz' match: 'mobaz' asWideString).
	self assert: ('*foo#zort' match: 'afoo3zortthenfoo3zort' asWideString).
	self assert: ('*baz' match: 'mobaz' asWideString).
	self assert: ('*foo#zort' match: 'afoo3zortthenfoo3zort' asWideString).
	
	self assert: ('*baz' asWideString match: 'mobaz' ).
	self assert: ('*foo#zort' asWideString match: 'afoo3zortthenfoo3zort' ).
	self assert: ('*baz' asWideString match: 'mobaz' ).
	self assert: ('*foo#zort' asWideString match: 'afoo3zortthenfoo3zort' ).
	
	self assert: ('*baz' asWideString match: 'mobaz' asWideString).
	self assert: ('*foo#zort' asWideString match: 'afoo3zortthenfoo3zort' asWideString).
	self assert: ('*baz' asWideString match: 'mobaz' asWideString).
	self assert: ('*foo#zort' asWideString match: 'afoo3zortthenfoo3zort' asWideString).
]

{ #category : #'tests - relation order' }
WideStringTest >> testRelationOrder [
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"

	self assert: ('aa' < 'ab').
	self assert: ('aa' <= 'ab').
	self assert: ('aa' <= 'aa').
	self assert: ('ab' > 'aa').
	self assert: ('ab' >= 'aa').
	self assert: ('aa' >= 'aa').
	
	self assert: ('aa' < 'ab' asWideString).
	self assert: ('aa' <= 'ab' asWideString).
	self assert: ('aa' <= 'aa' asWideString).
	self assert: ('ab' > 'aa' asWideString).
	self assert: ('ab' >= 'aa' asWideString).
	self assert: ('aa' >= 'aa' asWideString).
	
	self assert: ('aa' asWideString < 'ab').
	self assert: ('aa' asWideString <= 'ab').
	self assert: ('aa' asWideString <= 'aa').
	self assert: ('ab' asWideString > 'aa').
	self assert: ('ab' asWideString >= 'aa').
	self assert: ('aa' asWideString >= 'aa').
	
	self assert: ('aa' asWideString< 'ab' asWideString).
	self assert: ('aa' asWideString<= 'ab' asWideString).
	self assert: ('aa' asWideString<= 'aa' asWideString).
	self assert: ('ab' asWideString> 'aa' asWideString).
	self assert: ('ab' asWideString >= 'aa' asWideString).
	self assert: ('aa' asWideString>= 'aa' asWideString).
]

{ #category : #'tests - relation order' }
WideStringTest >> testRelationOrderWithCase [
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
	
]

{ #category : #'tests - compare' }
WideStringTest >> testSameAs [
	"from johnmci at http://bugs.squeak.org/view.php?id=5331"

	self assert: ('abc' sameAs: 'aBc' asWideString).
	self assert: ('aBc' asWideString sameAs: 'abc').
	self assert: ( #[97 0 0 0] asString sameAs: 'Abcd' asWideString) not.
	self assert: ('a000' asWideString sameAs:  #[97 0 0 0] asString) not.
	
]

{ #category : #'tests - substrings' }
WideStringTest >> testSubstrings [
	"this is related to http://bugs.squeak.org/view.php?id=6367"

	| w1 w2 |
	w1 := WideString
		with: 401 asCharacter
		with: $a
		with: 402 asCharacter
		with: $b.
	w2 := WideString with: 403 asCharacter with: 404 asCharacter.

	self assert: w1 substrings first equals: w1.
	self assert: (w1 , ' ' , w2) substrings size equals: 2.
	self assert: (w1 , ' ' , w2) substrings last equals: w2
]
