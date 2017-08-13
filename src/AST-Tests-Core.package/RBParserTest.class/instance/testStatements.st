tests parsing
testStatements
	| tree |
	#(('' 0 0) ('.' 0 1) ('| bar |' 0 0) ('| bar | .' 0 1) ('| bar | ..' 0 2) ('foo. bar' 2 1) ('foo. bar.' 2 2) ('foo. bar. .' 2 3) ('. foo. bar' 2 2)) do: [ :each |
		tree := self parserClass parseExpression: each first.
		self assert: tree statements size = each second.
		self assert: tree periods size = each last ]