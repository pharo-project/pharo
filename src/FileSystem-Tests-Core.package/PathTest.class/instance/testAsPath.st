tests
testAsPath
	| path pathFromString |
	path := Path * 'plonk'.
	pathFromString := 'plonk' asPath.
	self assert: path equals: pathFromString.
	
	path := Path / 'plonk'.
	pathFromString := '/plonk' asPath.
	self assert: path equals: pathFromString.