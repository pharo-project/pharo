tests
test12remove
	list add: link1.
	list add: link2.
	list add: link3.
	list add: link4.
	
	self assert: list size = 4.
	self assert: list first == link1.
	self assert: list second == link2.
	self assert: list third == link3.
	self assert: list fourth == link4.
	
	list remove: link3.
	self assert: list size = 3.
	self assert: list first == link1.
	self assert: list second == link2.
	self assert: list third == link4.
	
	list remove: link2.
	self assert: list size = 2.
	self assert: list first == link1.
	self assert: list second == link4.
	
	list remove: link1.
	self assert: list size = 1.
	self assert: list first == link4.
	
	list remove: link4.
	self assert: list isEmpty