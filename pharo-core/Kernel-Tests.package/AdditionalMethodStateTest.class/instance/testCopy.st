tests
testCopy

	| copy |
	copy := atState copy.

	self deny: atState == copy.
	self assert: atState method == copy method.
	self assert: atState selector == copy selector.

	self assert: atState pragmas = copy pragmas.
	self assert: atState properties = copy properties.

	1 to: atState pragmas size do: [:index | self deny: (atState pragmas at: index) == (copy pragmas at: index)].