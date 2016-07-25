method accessing
compile: aString withAttributesFrom: aRBMethod 
	| change method |
	change := model 
		compile: aString
		in: self
		classified: aRBMethod protocols.
	method := RBMethod 
		for: self
		source: aString
		selector: change selector.
	self addMethod: method.
	^ change