tests parsing
testArray
	| tree |
	#(('{}' 0 0) ('{.}' 0 1) ('{..}' 0 2) ('{foo. bar}' 2 1) ('{foo. bar.}' 2 2) ('{foo. bar. .}' 2 3) ('{. foo. bar}' 2 2) ('{foo.. bar}' 2 2))
	 do: [ :each |
		tree := self parserClass parseExpression: each first.
		self assert: tree statements size = each second.
		self assert: tree periods size = each last.
		self assert: tree left = 1.
		self assert: tree right = each first size ]