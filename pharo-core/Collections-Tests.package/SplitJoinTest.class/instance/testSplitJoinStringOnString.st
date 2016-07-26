running
testSplitJoinStringOnString
	self assert: ('oo' join: ('ow' split: 'how now brown cow?'))
		equals: 'hoo noo broon coo?'