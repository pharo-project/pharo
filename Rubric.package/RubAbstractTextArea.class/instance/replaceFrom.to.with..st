editing
replaceFrom: start to: stop with: aText
	self
		handleEdit: [ self editor replaceTextFrom: start to: stop with: aText ]
