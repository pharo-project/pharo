remapping
rememberReference: anInstruction to: name in: dictionary
	(dictionary at: name ifAbsentPut: [ OrderedCollection new ])
		add: anInstruction