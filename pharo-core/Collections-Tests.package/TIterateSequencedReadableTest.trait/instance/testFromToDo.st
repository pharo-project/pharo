tests - iterate on sequenced reable collections
testFromToDo
	
	| result |
	result:= OrderedCollection  new.
	
	self nonEmptyMoreThan1Element  from: 1 to: (self nonEmptyMoreThan1Element  size -1) do: [:each | result add: each].
	
	1 to: (self nonEmptyMoreThan1Element  size -1) do:
		[:i|
		self assert: (self nonEmptyMoreThan1Element  at:i )=(result at:i)].
	self assert: result size=(self nonEmptyMoreThan1Element  size-1).