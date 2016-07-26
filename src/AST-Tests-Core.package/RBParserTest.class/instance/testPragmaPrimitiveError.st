tests parsing
testPragmaPrimitiveError
	| tree  pragma |
	tree := RBParser 
				parseMethod: 'veryBasicAt: index 
	<primitive: #dooo module: #bar error: code>
	^ code'.
	
	self assert: tree pragmas size equals: 1.
	
	pragma := tree pragmas first.
	self assert: pragma selector equals: #primitive:module:error:.