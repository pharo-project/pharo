tests - basic
testCopy
	"self run: #testCopy"
	"self debug: #testCopy"
	
	|aSortedCollection copySorted|
	aSortedCollection := SortedCollection new.
	aSortedCollection sortBlock: [:a :b | a < b].
	aSortedCollection add: 'truite' ; add: 'brochet'.
	self assert: aSortedCollection first = 'brochet'.
	
	copySorted := aSortedCollection copy.
	
	self assert: (copySorted  hasEqualElements: aSortedCollection).
	self assert: (copySorted  species = aSortedCollection species).
	self assert: (copySorted  sortBlock = aSortedCollection sortBlock).
	self assert: copySorted first = 'brochet'.