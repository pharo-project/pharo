printing
printQueryOn: stream
	stream nextPut: $?. 
	ZnResourceMetaUtils writeQueryFields: self query on: 	stream