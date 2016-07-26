tests-accessing selection
testMarkBlockPointBlock
	self setSelectionFrom: 3 to: 6 text: area text.
	self assert: area pointBlock stringIndex equals: 3.
	self assert: area markBlock stringIndex equals: 6