tests - beginswith
testBeginsWith
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
