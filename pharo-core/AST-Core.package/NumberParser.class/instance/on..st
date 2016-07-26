initialize-release
on: aStringOrStream 
	sourceStream := aStringOrStream isString 
		ifTrue: [ aStringOrStream readStream ]
		ifFalse: [ aStringOrStream ].
	base := 10.
	neg := false.
	integerPart := fractionPart := exponent := scale := 0.
	requestor := failBlock := nil