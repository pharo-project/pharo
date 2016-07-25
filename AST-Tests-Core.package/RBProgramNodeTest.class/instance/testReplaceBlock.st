testing-replacing
testReplaceBlock
	| tree |
	tree := self parseMethod: 'run self foo ifNil: [ ^ true ]'.
	tree body statements first arguments first replaceWith: (self parseExpression: '[ ^ false ]').
	self
		assert:
			tree newSource
				=
					'run
	self foo ifNil: [ ^ false ]'