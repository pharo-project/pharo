tests - DictionaryIndexAccessing
testAt
	| collection association |
	collection := self nonEmpty .
	association := collection associations anyOne.
	
	self assert: (collection at: association key) = association value.