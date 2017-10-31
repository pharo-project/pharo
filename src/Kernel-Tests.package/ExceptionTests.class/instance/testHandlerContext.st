tests - handling
testHandlerContext
	"A test ensuring that when evaluating the action block the exception environment is set to the handler context."

	| result |
	result := [ 
	[ 
	[ MyResumableTestError signal ]
		on: MyTestError
		do: [ 'handler 2' ] ]
		on: MyResumableTestError
		do: [ MyTestError signal ] ]
		on: MyTestError
		do: [ 'handler 1' ].
	self assert: 'handler 1' = result description: 'Incorrect handler'