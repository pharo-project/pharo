tests
test02addLast
	self assert: list isEmpty.
	
	list addLast: link1.
	self assert: list size = 1.
	self assert: list first = link1.
	
	list addLast: link2.
	self assert: list size = 2.
	self assert: list first = link1.
	self assert: list second = link2.
	
	list addLast: link3.
	self assert: list size = 3.
	self assert: list first = link1.
	self assert: list second = link2.
	self assert: list third = link3.
	
	list addLast: link4.
	self assert: list size = 4.
	self assert: list first = link1.
	self assert: list second = link2.
	self assert: list third = link3.
	self assert: list fourth = link4