tests - DictionaryIndexAccessing
testAtIfPresentIfAbsent
	"self run: #testAtIfPresentIfAbsent"

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
				ifAbsent: [ Error signal ]) = #present.
	self assert: arg = association value.

	"ensure cull: is used to support blocks that take no args"
	self
		assert:
			(collection
				at: association key
				ifPresent: [ #present ]
				ifAbsent: [ Error signal ]) = #present.
	self
		assert:
			(collection
				at: self keyNotIn
				ifPresent: [ :value | Error signal ]
				ifAbsent: [ self valueNotIn ]) = self valueNotIn