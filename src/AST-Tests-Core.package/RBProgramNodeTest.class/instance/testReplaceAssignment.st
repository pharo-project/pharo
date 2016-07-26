testing-replacing
testReplaceAssignment
	| tree |
	tree := self parseMethod: 'run sum := 2 + 2'.
	tree body statements first replaceWith: (self parseExpression: 'multpppp := 2 * 2').
	self
		assert:
			tree newSource
				=
					'run
	multpppp := 2 * 2'