convencience
toJsonString: object
	^ String streamContents: [ :stream |
		self put: object asJsonOnStream: stream ]