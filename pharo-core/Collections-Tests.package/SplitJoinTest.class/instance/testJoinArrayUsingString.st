running
testJoinArrayUsingString
	self assert: ((1 to: 4) joinUsing: '--')
		equals: '1--2--3--4'