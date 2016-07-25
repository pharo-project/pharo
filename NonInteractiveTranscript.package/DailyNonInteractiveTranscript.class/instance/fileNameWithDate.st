accessing
fileNameWithDate
	| newName |
	newName := self fileName format: { date yyyymmdd }.
	self 
		assert: self fileName ~= newName 
		description: 'The filename should include a {1} slot for the date'.
	^ newName