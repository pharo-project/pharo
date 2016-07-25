private-formatting
formatArray: anArrayNode
	anArrayNode statements
		do: [ :each | 
				self visitNode: each ]
		separatedBy: 
				[ codeStream nextPutAll: ' . ' .
				self newLine ]