tests - DictionaryIndexAccessing
testAtIfAbsentPut
	"self run: #testAtIfAbsentPut"

	| collection association |
	collection := self nonEmpty.
	association := collection associations anyOne.
	self
		assert:
			(collection at: association key ifAbsentPut: [ Error signal ])
				= association value.
	self assert: (collection at: association key) = association value.
	self
		assert:
			(collection at: self keyNotIn ifAbsentPut: [ self valueNotIn ])
				= self valueNotIn.
	self assert: (collection at: self keyNotIn) = self valueNotIn