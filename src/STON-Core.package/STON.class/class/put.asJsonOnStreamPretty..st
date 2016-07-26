convencience
put: object asJsonOnStreamPretty: stream
	(self jsonWriter on: stream)
		prettyPrint: true; 
		nextPut: object