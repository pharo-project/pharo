actions
choose
	self chooseReference ifNotNil: [ :reference | 
		self location: reference.
		self locationInput text: reference pathString ]