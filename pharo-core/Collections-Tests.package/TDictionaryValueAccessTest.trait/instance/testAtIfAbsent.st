tests - DictionaryIndexAccessing
testAtIfAbsent
	| collection association |
	collection := self nonEmpty .
	association := collection associations anyOne.
	
	self assert: (collection at: association key ifAbsent: [ 888 ]) = association value.
	self assert: (collection at: self keyNotIn  ifAbsent: [ 888 ]) = 888.