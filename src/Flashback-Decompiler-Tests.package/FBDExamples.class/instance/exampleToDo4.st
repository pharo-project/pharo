examples - loops
exampleToDo4
	| tmp1 tmp2 |
	tmp1 := {true . true . true }.
	tmp2 := tmp1 size.
	1 to: tmp2 do: [ :index | tmp1 at: index put: false ].
	1 to: tmp2 do: [ :index | tmp1 at: index put: true ].
	^ tmp1