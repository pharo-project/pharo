error handling
scannerError: aString
	(self errorBlock cull: aString cull: self errorPosition cull: self) ifNil: [ 
		^ SyntaxErrorNotification
					inClass: Object
					withCode: stream contents
					doitFlag: false
					errorMessage: aString
					location: stream position + 1
	]