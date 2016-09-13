tests - DictionaryIndexAccessing
testAtIfPresent
	"self run: #testAtIfPresent"

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
					#present ]) = #present.
	self assert: arg = association value.

	"ensure cull: is used to support blocks that take no args"
	self
		assert: (collection at: association key ifPresent: [ #present ]) = #present.
	self
		assert:
			(collection
				at: self keyNotIn
				ifPresent: [ :value | Error signal ]) isNil