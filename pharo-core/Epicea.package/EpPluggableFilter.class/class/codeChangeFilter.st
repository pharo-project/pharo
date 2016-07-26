well-known filters
codeChangeFilter
	^ self
		title: 'any code change'
		condition: [ :e | e content isCodeChange ]