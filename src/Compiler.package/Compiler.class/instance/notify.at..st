error handling
notify: aString at: location
	"Refer to the comment in Object|notify:."

	^requestor == nil
		ifTrue: [SyntaxErrorNotification
					inClass: class
					category: category
					withCode: sourceStream contents
					doitFlag: false
					errorMessage: aString
					location: location]
		ifFalse: [requestor
					notify: aString
					at: location
					in: sourceStream]