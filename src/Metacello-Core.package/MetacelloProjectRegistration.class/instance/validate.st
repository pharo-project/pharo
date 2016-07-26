accessing
validate
	self isValid
		ifFalse: [ self error: 'Invalid project registration' ]