well-known filters
codeChangeFilter
	^ self
		title: 'is code change'
		condition: [ :e | e content isCodeChange ]