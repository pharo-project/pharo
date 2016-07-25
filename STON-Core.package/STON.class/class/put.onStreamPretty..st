convencience
put: object onStreamPretty: stream
	(self writer on: stream)
		prettyPrint: true; 
		nextPut: object