tests
testCorrespondingTestMethod

	self assert: ((Dictionary>>#associations) correspondingTestMethod == (DictionaryTest>>#testAssociations)).
	self assert: ((Dictionary>>#add:) correspondingTestMethod == (DictionaryTest>>#testAdd)).
	self assert: ((Dictionary>>#at:put:) correspondingTestMethod == (DictionaryTest>>#testAtPut)).