tests
test14removeIfAbsent
	list add: link1.
	
	self assert: list size = 1.
	self assert: list first == link1.
	
	list remove: link1.
	self assert: list isEmpty.
	
	[list remove: link1]
		on: Error
		do: [^ self].
		
	"The execution should not get here. If yes, something went wrong."
	self assert: false