tests - DictionaryIndexAccessing
testAtIfAbsent
	"self run: #testAtIfAbsent"

	| collection association |
	collection := self nonEmpty.
	association := collection associations anyOne.
	self
		assert:
			(collection at: association key ifAbsent: [ Error signal ])
				= association value.
	self
		assert:
			(collection at: self keyNotIn ifAbsent: [ self valueNotIn ])
				= self valueNotIn