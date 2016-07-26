accessing
getText
	^ self method ifNil: [''] ifNotNil: [:m | m sourceCode]