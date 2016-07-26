correcting
notify: aString at: location
	"Refer to the comment in Object|notify:."

	^self requestor 
		ifNil: [SyntaxErrorNotification
					inClass: self methodClass 
					category: self methodClass category
					withCode: self methodNode source
					doitFlag: false
					errorMessage: aString
					location: location]
		ifNotNil: [self requestor
					notify: aString , ' ->'
					at: location
					in: self requestor text]