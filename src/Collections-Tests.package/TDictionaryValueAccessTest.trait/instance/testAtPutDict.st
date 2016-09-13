tests - DictionaryIndexAccessing
testAtPutDict
	"Do not rename to testAtPut, will conflict with TPutBasicTest>>#testAtPut"

	"self run: #testAtPutDict"

	"self debug: #testAtPutDict"

	| collection keyIn newValue |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.
	newValue := self valueNotIn.

	"use == to ensure the exact object argument to put: is stored and returned"
	self assert: (collection at: keyIn put: newValue) == newValue.
	self assert: (collection at: keyIn) == newValue.

	newValue := self valueNotIn -> self valueNotIn.
	self assert: (collection at: keyIn put: newValue) == newValue.
	self assert: (collection at: keyIn) == newValue.

	newValue := {self valueNotIn.
	self valueNotIn.
	self valueNotIn}.
	self assert: (collection at: self keyNotIn put: newValue) == newValue.
	self assert: (collection at: self keyNotIn) == newValue