well-known filters
codeChangeFilter
	^ self
		title: 'Show only code changes'
		condition: [ :e | e content isCodeChange ]