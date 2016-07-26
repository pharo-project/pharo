accessing
code: integer
	(ZnConstants httpStatusCodes includesKey: integer) 
		ifFalse: [ 
			(ZnUnknownHttpStatusCode code: integer) signal ].
	code := integer