tests-accessing selection
testModifySelection
	self testMarkBlockPointBlock.
	area modifySelection: (self newCharacterBlockIndex: 8 text: area text).
	self assert: area pointBlock stringIndex equals: 3.
	self assert: area markBlock stringIndex equals: 8.