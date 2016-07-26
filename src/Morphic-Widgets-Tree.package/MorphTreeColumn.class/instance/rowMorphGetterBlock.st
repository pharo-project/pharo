column drawing
rowMorphGetterBlock
	^ self rowMorphGetSelector isSymbol
		ifTrue: [self rowMorphGetSelector numArgs = 0
				ifTrue: [[:node :cont | node perform: self rowMorphGetSelector]]
				ifFalse: [[:node :cont| node perform: self rowMorphGetSelector with: self container]]]
		ifFalse: [self rowMorphGetSelector numArgs = 2
				ifTrue: [self rowMorphGetSelector]
				ifFalse: [[:node :cont| self rowMorphGetSelector value: node]]] 