tests parsing
testPragmaPrimitiveError
	| tree  pragma |
	tree := self parserClass 
				parseMethod: 'veryBasicAt: index 
	<primitive: #dooo module: #bar error: code>
	^ code'.
	
	self assert: tree pragmas size equals: 1.
	
	pragma := tree pragmas first.
	self assert: pragma selector equals: #primitive:module:error:.