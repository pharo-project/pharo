accessing
reason
	reason ifNil: [ 
		reason := ZnConstants httpStatusCodes 
			at: self code 
			ifAbsent: [ 'Unknown Status Code' ] ].
	^ reason