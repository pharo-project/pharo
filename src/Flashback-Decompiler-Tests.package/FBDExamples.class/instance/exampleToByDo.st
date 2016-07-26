examples - loops
exampleToByDo
	| tmp1 |
	tmp1 := {true.	true.	true}.
	1 to: 9 by: 3 do: [ :index | tmp1 at: index / 3  put: false ].
	^ tmp1