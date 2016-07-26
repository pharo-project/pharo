tests
testCorrespondingMethods

	self assert: ((Dictionary>>#associations) == (DictionaryTest>>#testAssociations) correspondingMethods first).
	self assert: ((Dictionary>>#add:) == (DictionaryTest>>#testAdd) correspondingMethods first).
	self assert: ((Dictionary>>#at:put:) == (DictionaryTest>>#testAtPut) correspondingMethods first).