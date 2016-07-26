attributes
wordSize
	^ WordSize ifNil: [WordSize := [self parameterAt: 40] on: Error do: [4]]