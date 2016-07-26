convencience
toStringPretty: object
	^ String streamContents: [ :stream |
		self put: object onStreamPretty: stream ]