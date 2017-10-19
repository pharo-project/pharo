tests
test05addBefore
	self assert: list isEmpty.
	
	list add: link1.
	self assert: list size = 1.
	self assert: list first == link1.
	
	list add: link2 before: link1.
	self assert: list size = 2.
	self assert: list first == link2.
	self assert: list second == link1.
	
	list add: link3 before: link2.
	self assert: list size = 3.
	self assert: list first == link3.
	self assert: list second == link2.
	self assert: list third == link1.
	
	list add: link4 before: link3.
	self assert: list size = 4.
	self assert: list first == link4.
	self assert: list second == link3.
	self assert: list third == link2.
	self assert: list fourth == link1