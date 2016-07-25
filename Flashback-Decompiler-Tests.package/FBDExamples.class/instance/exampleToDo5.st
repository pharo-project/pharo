examples - loops
exampleToDo5
	| tmp1 |
	tmp1 := {true.
	true.
	true}.
	[ 1 to: 3 do: [ :index | tmp1 at: index put: false ] ] valueWithin: 50 milliSeconds onTimeout: [  ].
	^ tmp1