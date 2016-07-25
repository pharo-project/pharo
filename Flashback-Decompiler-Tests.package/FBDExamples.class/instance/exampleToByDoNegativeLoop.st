examples - loops
exampleToByDoNegativeLoop
	| tmp1 |
	tmp1 := {true.	true.	true}.
	tmp1 size to: 10-9 by: -1 do: [ :index | tmp1 at: index put: false ].
	^ tmp1