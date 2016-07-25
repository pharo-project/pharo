tests-model basics
testReplaceAllWith
	| replacementText |
	text := 'hello' asTxModel.
	replacementText := ' world' asTxModel.
	text replaceAllWith: replacementText.
	self assert: text asString equals: replacementText asString.