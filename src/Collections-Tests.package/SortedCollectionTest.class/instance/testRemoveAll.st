tests - basic
testRemoveAll
	"Allows one to remove all elements of a collection" 
	
	| c1 c2 s2 |
	c1 := #(10 9 8 7 5 4 4 2) asSortedCollection: [:a :b | a >= b].
	c2 := c1 copy.
	s2 := c2 size.
	
	c1 removeAll.
	
	self assert: c1 size = 0.
	self assert: c2 size = s2 description: 'the copy has not been modified'.
	
	c1 add: 13; add: 14.
	self assert: (c1 first = 14 and: [c1 second = 13])  description: 'the sortBlock has been preserved'.