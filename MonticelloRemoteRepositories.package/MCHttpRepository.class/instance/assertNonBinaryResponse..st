private
assertNonBinaryResponse: response 
	
	response contentType isBinary ifTrue: [ MCRepositoryError signal: 'Did not expect a binary response but got ', response contentType printString ].