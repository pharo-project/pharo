tests - DictionaryIndexAccessing
testAtIfAbsentPut
	| collection association |
	collection := self nonEmpty .
	association := collection associations anyOne.
	
	collection at: association key ifAbsentPut: [ 888 ].
	self assert: (collection at: association key) = association value.
	
	collection at: self keyNotIn  ifAbsentPut: [ 888 ].
	self assert: ( collection at: self keyNotIn ) = 888.