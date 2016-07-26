accessing
errorCode
	errorCode
		ifNotNil: [^ errorCode].
	^ errorCode := [FT2Library errorCode]
				on: Error
				do: [:ex | ex return: -1]