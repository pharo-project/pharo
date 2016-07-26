tests - iterate on sequenced reable collections
testReverseDo
	| result |
	result:= OrderedCollection new.
	self nonEmpty reverseDo: [: each | result add: each].
	
	1 to: result size do:
		[:i|
		self assert: (result at: i)=(self nonEmpty at:(self nonEmpty size-i+1))].