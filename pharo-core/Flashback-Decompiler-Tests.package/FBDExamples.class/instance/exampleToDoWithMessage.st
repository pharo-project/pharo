examples - loops
exampleToDoWithMessage
	| t |
	t := {true.true.true}.
	^ 1 yourself to: t size do: [ :index | t at: index put: false ]