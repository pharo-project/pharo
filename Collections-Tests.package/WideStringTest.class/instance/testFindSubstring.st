tests - substrings
testFindSubstring
	"This is related to http://bugs.squeak.org/view.php?id=6366
	finding substring in a WideString was broken because matchTable are byte-wise"
	
	| ws1 ws2 |
	
	self assert: ('abcd' findString: 'bc' startingAt: 1) = 2.
	self assert: ('abcd' asWideString findString: 'bc' startingAt: 1) = 2.
	self assert: ('abcd' findString: 'bc' asWideString startingAt: 1) = 2.
	self assert: ('abcd' asWideString findString: 'bc' asWideString startingAt: 1) = 2.
	
	ws1 := 'A' , (WideString with: (Unicode value: 530)) , 'BCD'.
	self assert: (ws1 findString: 'bc' startingAt: 1 caseSensitive: true) = 0.
	self assert: (ws1 findString: 'bc' startingAt: 1 caseSensitive: false) = 3.
	
	ws2 := (WideString with: (Unicode value: 530)) , 'b'.
	self assert: (ws1 findString: ws2 startingAt: 1 caseSensitive: true) = 0.
	self assert: (ws1 findString: ws2 startingAt: 1 caseSensitive: false) = 2.
	
	self assert: ('abc' findString: ws2 startingAt: 1 caseSensitive: true) = 0.
	self assert: ('abc' findString: ws2 startingAt: 1 caseSensitive: false) = 0.