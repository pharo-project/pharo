convencience
toString: object
	^ String streamContents: [ :stream |
		self put: object onStream: stream ]