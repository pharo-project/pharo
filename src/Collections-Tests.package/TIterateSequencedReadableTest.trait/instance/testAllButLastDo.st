tests - iterate on sequenced reable collections
testAllButLastDo
	
	| result |
	result:= OrderedCollection  new.
	
	self nonEmptyMoreThan1Element  allButLastDo: [:each | result add: each].
	
	1 to: (result size) do:
		[:i|
		self assert: (self nonEmptyMoreThan1Element  at:(i ))=(result at:i)].
	
	self assert: result size=(self nonEmptyMoreThan1Element  size-1).