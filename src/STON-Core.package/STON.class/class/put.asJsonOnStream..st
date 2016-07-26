convencience
put: object asJsonOnStream: stream
	(self jsonWriter on: stream) nextPut: object