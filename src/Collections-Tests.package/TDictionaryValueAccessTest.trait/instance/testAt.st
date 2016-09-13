tests - DictionaryIndexAccessing
testAt
	"self run: #testAt"

	| collection association |
	collection := self nonEmpty.
	association := collection associations anyOne.
	self assert: (collection at: association key) = association value.
	self should: [ collection at: self keyNotIn ] raise: Error