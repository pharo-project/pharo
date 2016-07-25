drag and drop
dropElements: aCollection index: rowIndex
	self dropItemSelector ifNil: [ ^ self ].
	self model 
		perform: self dropItemSelector 
		with: aCollection
		with: rowIndex