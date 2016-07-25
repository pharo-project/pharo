examples - loops
exampleToDo3
	| t |
	t := Array new:3.
	1 to: 3 do: [ :index | t at: index put: false ].
	^ t