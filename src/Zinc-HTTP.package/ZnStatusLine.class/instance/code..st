accessing
code: integer
	(ZnConstants httpStatusCodes includesKey: integer) 
		ifFalse: [
			(integer between: 100 and: 599)
				ifFalse: [
					(ZnUnknownHttpStatusCode code: integer) signal ] ].
	code := integer