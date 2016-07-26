examples
constructSharedClosureEnvironmentInDeadFrame

	|array result| 
	result := 10.
	array := Array new: 2.
	array at: 1 put: [:arg | result := arg].
	array at: 2 put: [result].
	^array
