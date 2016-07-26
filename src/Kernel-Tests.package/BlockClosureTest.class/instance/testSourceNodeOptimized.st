tests - astmapping
testSourceNodeOptimized
	| block | 
	block := [ :ctx | [ ctx atEnd ] whileTrue:[1+2 ] ] .
	self assert: block sourceNode printString = 'RBBlockNode([ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ])'.
