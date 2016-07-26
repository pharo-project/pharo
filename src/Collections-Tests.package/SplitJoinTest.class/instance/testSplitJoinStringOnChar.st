running
testSplitJoinStringOnChar
	self assert: ($: join: (Character space split: 'how now brown cow?'))
		equals: 'how:now:brown:cow?'