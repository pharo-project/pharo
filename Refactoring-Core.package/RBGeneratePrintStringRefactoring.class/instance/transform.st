transforming
transform
	| method |
	method := RBParser parseMethod: 'printOn: aStream
		"Append a sequence of characters to aStream that identify the receiver."
		
		super printOn: aStream'.
	variables do: [ :each |
		method body addNode: (RBParser 
			parseExpression: ('aStream nextPutAll: '' <1s>: ''; print: <1s>'
				expandMacrosWith: each)) ].
	self theClass
		compile: method formattedCode
		classified: #(printing)