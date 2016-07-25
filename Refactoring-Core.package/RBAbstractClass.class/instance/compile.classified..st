method accessing
compile: aString classified: aSymbolCollection 
	| change method |
	change := model 
				compile: aString
				in: self
				classified: aSymbolCollection.
	method := RBMethod 
				for: self
				source: aString
				selector: change selector.
	self addMethod: method.
	^ change