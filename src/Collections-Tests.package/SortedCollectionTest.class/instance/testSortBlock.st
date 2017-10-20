tests - basic
testSortBlock
	"self run: #testSortBlock"
	"self debug: #testSortBlock"
	
	|aSortedCollection|
	aSortedCollection := SortedCollection new.
	aSortedCollection sortBlock: [:a :b | a < b].
	aSortedCollection add: 'truite' ; add: 'brochet' ; add: 'tortue'.
	self assert: aSortedCollection first = 'brochet'.
	
	aSortedCollection := SortedCollection new.
	aSortedCollection sortBlock: [:a :b | a >b].
	aSortedCollection add: 'truite' ; add: 'brochet' ; add: 'tortue'.
	self assert: aSortedCollection first = 'truite'.
	
	
	