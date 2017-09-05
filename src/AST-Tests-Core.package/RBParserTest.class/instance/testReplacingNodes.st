tests
testReplacingNodes
	| tree search block |
	tree := self parserClass 
				parseMethod: '+ a | a b | self ifTrue: [a] ifFalse: [b := c]. a := b. [:b :c :a | a foo: a; foo1: a; foo2: a foo: b]. {a. b}. ^a'.
	search := RBParseTreeSearcher new.
	block := [:aNode :answer | aNode replaceWith: (RBVariableNode named: 'q')].
	search
		matches: 'a' do: block;
		matchesArgument: 'a' do: block.
	search executeTree: tree.
	self 
		assert: tree = (self parserClass 
						parseMethod: '+ q | q b | self ifTrue: [q] ifFalse: [b := c]. q := b. [:b :c :q | q foo: q; foo1: q; foo2: q foo: b]. {q. b}. ^q').
	self 
		assert: tree removeDeadCode = (self parserClass 
						parseMethod: '+ q | q b | self ifTrue: [] ifFalse: [b := c]. q := b. {q. b}. ^q')