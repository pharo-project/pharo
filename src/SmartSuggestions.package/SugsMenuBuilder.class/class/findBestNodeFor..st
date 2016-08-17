private
findBestNodeFor: context
	| root node |
	"since the ast is used to put links for break points reparsing it will lose all the 
	meta information such as breakpoints, therefore when the code is unchanged do not reparse it."
	context hasUnacceptedEdits
		ifTrue: [ 
			root := Smalltalk compiler
				source: context code;
				options: #(+ optionParseErrors);
				parse.
			context selectedClass ifNotNil: [ :theClass | root methodNode methodClass: theClass ].
			root doSemanticAnalysis ]
		ifFalse: [ root := context selectedMethod ast ].
	node := root bestNodeFor: context selectedInterval.
	context selectedNode: node.
	^ node