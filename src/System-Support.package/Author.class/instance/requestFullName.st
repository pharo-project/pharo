ui-requests
requestFullName
	| initialAnswer |
	
	initialAnswer := fullName isEmptyOrNil
				ifTrue: ['FirstnameLastname']
				ifFalse: [fullName].
				
	(AuthorNameRequest new initialAnswer: initialAnswer) signal ifNotNil: [ :newName | 
		self checkAndSetFullNameWith: newName 
	] "nil means that the dialog was cancelled "