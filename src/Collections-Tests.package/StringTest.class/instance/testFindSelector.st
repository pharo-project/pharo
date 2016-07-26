tests - iterate on sequenced reable collections
testFindSelector
	self assert: #isSymbol equals: 'isSymbol' findSelector.
	self assert: #between:and: equals: 'between:and:' findSelector .
	self assert: #+ equals: '+' findSelector .
	self assert: #isSymbol equals: '#isSymbol' findSelector.
	self assert: #between:and: equals: '#between:and:' findSelector .
	self assert:#+ equals: '#+' findSelector
