accessing
errorString
	errorString
		ifNotNil: [^ errorString].
	^ errorString := [FT2Library errorString]
				on: Error
				do: [:ex | ex return: 'can''t get error string']