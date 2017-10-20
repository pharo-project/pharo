tests - handling
testHandlerFromAction
	"A test ensuring that nested exceptions work as expected."

	| result |
	result := [ 
	[ 
	[ self error: 'trigger error' ]
		on: ZeroDivide
		do: [ :ex | 'inner' ] ]
		on: Error
		do: [ :ex | 3 / 0 ] ]
		on: ZeroDivide
		do: [ :ex | 'outer' ].
	self assert: 'outer' = result description: 'Incorrect handler'