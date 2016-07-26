tests - iterate on sequenced reable collections
testReverseDoEmpty
	| result |
	result:= OrderedCollection new.
	self empty reverseDo: [: each | result add: each].
	
	self assert: result isEmpty .