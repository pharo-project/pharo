tests - DictionaryIndexAccessing
testAtError
	"self run: #testAtError"

	| dict nonExistantKey keyIn |
	dict := self nonEmpty.
	nonExistantKey := self keyNotIn.
	keyIn := dict keys anyOne.
	dict at: keyIn.
	self should: [ dict at: nonExistantKey ] raise: Error