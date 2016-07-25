convencience
toJsonStringPretty: object
	^ String streamContents: [ :stream |
		self put: object asJsonOnStreamPretty: stream ]