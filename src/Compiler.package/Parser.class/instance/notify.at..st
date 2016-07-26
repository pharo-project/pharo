error handling
notify: string at: location
	requestor isNil
		ifTrue: [(encoder == self or: [encoder isNil]) ifTrue: [^ self fail  "failure setting up syntax error"].
				SyntaxErrorNotification
					inClass: encoder classEncoding
					category: category
					withCode: source contents
					doitFlag: doitFlag
					errorMessage: string
					location: location]
		ifFalse: [requestor
					notify: string , ' ->'
					at: location
					in: source].
	^self fail