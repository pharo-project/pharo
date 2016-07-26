examples - loops
exampleToDo2
	| t |
	t := {true.true.true}.
	1 to: t size do: [ :index | t at: index put: false ].
	^ t