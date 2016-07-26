tests
testStartForReplacement02

	| source tree dTemporary |
	source := 'foo
	|a b c de 
	
d|
	de := 4'.
	tree := RBParser parseMethod: source.
	dTemporary := tree body temporaries last.
	self assert: dTemporary name equals: #d.
	self assert: (source copyFrom: dTemporary start to: dTemporary stop) equals: 'd'.
	self assert: (source copyFrom: dTemporary startForReplacement to: dTemporary stop) equals: ' 
	
d'.
	self assert: (dTemporary start - dTemporary startForReplacement) equals: 4