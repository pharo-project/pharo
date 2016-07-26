examples-blocks-optimized
exampleToDoWithMessage2
	| t t1 t2 |
	t := {true.true.true}.
	t1 := 1.
	t2 := t size.
	t1 to: t2 do: [ :index | t at: index put: false ]