tests - empty
testIfEmpty

	self nonEmpty ifEmpty: [ self assert: false] .
	self empty ifEmpty: [ self assert: true] .
	

	