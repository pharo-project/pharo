tests - copying with or without
testCopyWithoutFirst

	| result |
	result := self nonEmpty copyWithoutFirst.
	
	self assert: result size = (self nonEmpty size - 1).
	
	1 to: result size do:
		[:i |
		self assert: (result at: i)= (self nonEmpty at: (i + 1))].