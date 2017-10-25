tests
test11removeLast
	list add: link1.
	list add: link2.
	list add: link3.
	list add: link4.
	
	self assert: list size = 4.
	self assert: list first == link1.
	self assert: list second == link2.
	self assert: list third == link3.
	self assert: list fourth == link4.
	
	list removeLast.
	self assert: list size = 3.
	self assert: list first == link1.
	self assert: list second == link2.
	self assert: list third == link3.
	
	list removeLast.
	self assert: list size = 2.
	self assert: list first == link1.
	self assert: list second == link2.
	
	list removeLast.
	self assert: list size = 1.
	self assert: list first == link1.
	
	list removeFirst.
	self assert: list isEmpty