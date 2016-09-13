tests - DictionaryIndexAccessing
testAtIfPresentIfAbsentPut
	"self run: #testAtIfPresentIfAbsentPut"

	| collection association arg |
	collection := self nonEmpty.
	association := collection associations anyOne.
	arg := nil.
	self
		assert:
			(collection
				at: association key
				ifPresent: [ :value | 
					arg := value.
					#present ]
				ifAbsentPut: [ Error signal ]) = #present.
	self assert: arg = association value.

	"ensure cull: is used to support blocks that take no args"
	self
		assert:
			(collection
				at: association key
				ifPresent: [ #present ]
				ifAbsentPut: [ Error signal ]) = #present.
	self
		assert:
			(collection
				at: self keyNotIn
				ifPresent: [ :value | Error signal ]
				ifAbsentPut: [ self valueNotIn ]) = self valueNotIn.
	self assert: (collection at: self keyNotIn) = self valueNotIn