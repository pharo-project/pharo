error
expected: aString 
	| errorString |
	errorString := aString , ' expected'.
	requestor isNil
		ifFalse: [requestor
				notify: errorString
				at: sourceStream position + 1
				in: sourceStream].
	failBlock ifNotNil: [^failBlock cull: errorString cull: sourceStream position + 1].
	self error: 'Reading a number failed: ' , errorString