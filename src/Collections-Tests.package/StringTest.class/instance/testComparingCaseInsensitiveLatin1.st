tests - comparing
testComparingCaseInsensitiveLatin1

	"ascii-letter"
	self assert: ('e' compare: 'E' caseSensitive: false) equals: 2.
	self assert: ('E' compare: 'e' caseSensitive: false) equals: 2.
	self assert: ('e' compare: 'E' caseSensitive: true) equals: 3.
	self assert: ('E' compare: 'e' caseSensitive: true) equals: 1.
	
	"ascii-number (no difference between number chars and number chars asUppercase "
	self assert: ('1' compare: '1' asUppercase caseSensitive: false) equals: 2.
	self assert: ('1' asUppercase compare: '1' caseSensitive: false) equals: 2.
	self assert: ('1' compare: '1' asUppercase caseSensitive: true) equals: 2.
	self assert: ('1' asUppercase compare: '1' caseSensitive: true) equals: 2.
	
	"non-ascii"
	self assert: ('é' compare: 'É' caseSensitive: false) equals: 2.
	self assert: ('É' compare: 'é' caseSensitive: false) equals: 2.
	self assert: ('é' compare: 'É' caseSensitive: true) equals: 3.
	self assert: ('É' compare: 'é' caseSensitive: true) equals: 1
