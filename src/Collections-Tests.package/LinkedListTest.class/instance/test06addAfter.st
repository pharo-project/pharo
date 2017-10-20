tests
test06addAfter
	self assert: list isEmpty.
	
	list add: link1.
	self assert: list size = 1.
	self assert: list first == link1.
	
	list add: link2 after: link1.
	self assert: list size = 2.
	self assert: list first == link1.
	self assert: list second == link2.
	
	list add: link3 after: link2.
	self assert: list size = 3.
	self assert: list first == link1.
	self assert: list second == link2.
	self assert: list third == link3.
	
	list add: link4 after: link3.
	self assert: list size = 4.
	self assert: list first == link1.
	self assert: list second == link2.
	self assert: list third == link3.
	self assert: list fourth == link4