accessing
at: key put: value
	first ifTrue: [ first := false ] ifFalse: [ writer mapElementSeparator ].
	writer encodeKey: key value: value