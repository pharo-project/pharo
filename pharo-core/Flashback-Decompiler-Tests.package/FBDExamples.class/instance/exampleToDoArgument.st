examples-blocks-optimized
exampleToDoArgument
	collection := OrderedCollection new.
	1 to: 5 do: [ :index |
		collection add:  [index]  ].
	 ^collection collect: #value. 
	"#(1 2 3 4 5)"