accessing
selectedMessageName
	^methodSelection ifNotNil: [^ methodSelection selector ].
