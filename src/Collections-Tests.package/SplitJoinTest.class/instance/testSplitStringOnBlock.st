running
testSplitStringOnBlock
	self assert: ('foobar' splitOn: [:ch | 'aeiou' includes: ch])
		equals: #('f' '' 'b' 'r') asOrderedCollection