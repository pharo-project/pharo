tests - iterate on sequenced reable collections
testFindFirstNotIn

	| result |
	
	 result:=self empty findFirst: [:each | true].
	
	self assert: result=0. 