private
assertBinaryResponse: response 

	response contentType isBinary ifFalse: [ 
		MCRepositoryError signal: 'Expected a binary response instead of ', response contentType printString ].