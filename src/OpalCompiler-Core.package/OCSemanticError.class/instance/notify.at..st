error handling
notify: aString at: location
	"Refer to the comment in Object|notify:."

	^compilationContext requestor 
		ifNil: [SyntaxErrorNotification
					inClass: self methodClass 
					withCode: self methodNode source
					doitFlag: false
					errorMessage: aString
					location: location]
		ifNotNil: [compilationContext requestor
					notify: aString , ' ->'
					at: location
					in: compilationContext requestor text]