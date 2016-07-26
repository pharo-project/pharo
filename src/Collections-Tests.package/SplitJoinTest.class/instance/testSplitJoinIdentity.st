running
testSplitJoinIdentity
	| array string |
	array := #(5 1 4 1 3 1 2 1).
	string := 'how now brown cow'.
	self assert: (1 join: (1 split: array)) equals: array.
	self assert: (#(1 3) join: (#(1 3) split: array)) equals: array.
	self assert: ($o join: ($o split: string)) equals: string.
	self assert: ('ow' join: ('ow' split: string)) equals: string.