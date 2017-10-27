tests
test07addAfter
	self assert: list isEmpty.
	
	list add: link1.
	self assert: list size = 1.
	self assert: list first == link1.
	
	list add: link2 after: link1.
	self assert: list size = 2.
	self assert: list first == link1.
	self assert: list second == link2.
	
	list add: link3 after: link1.
	self assert: list size = 3.
	self assert: list first == link1.
	self assert: list second == link3.
	self assert: list third == link2.
	
	list add: link4 after: link1.
	self assert: list size = 4.
	self assert: list first == link1.
	self assert: list second == link4.
	self assert: list third == link3.
	self assert: list fourth == link2