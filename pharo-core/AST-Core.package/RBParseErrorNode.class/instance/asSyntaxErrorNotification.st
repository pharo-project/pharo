converting
asSyntaxErrorNotification
	^SyntaxErrorNotification new
		setClass: self methodNode methodClass
		category: nil 
		code: self methodNode source
		doitFlag: false
		errorMessage: errorMessage
		location: self start