tests
testRemoveAll
	"Allows one to remove all elements of a collection" 
	
	| c1 c2 s2 |
	c1 := full.
	c2 := c1 copy.
	s2 := c2 size.
	
	c1 removeAll.
	
	self assert: c1 size = 0.
	self assert: c2 size = s2 description: 'the copy has not been modified'.