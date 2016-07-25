tests - iterate on sequenced reable collections
testAllButFirstDo
	
	| result |
	result:= OrderedCollection  new.
	
	self nonEmptyMoreThan1Element  allButFirstDo: [:each | result add: each].
	
	1 to: (result size) do:
		[:i|
		self assert: (self nonEmptyMoreThan1Element  at:(i +1))=(result at:i)].
	
	self assert: result size=(self nonEmptyMoreThan1Element  size-1).