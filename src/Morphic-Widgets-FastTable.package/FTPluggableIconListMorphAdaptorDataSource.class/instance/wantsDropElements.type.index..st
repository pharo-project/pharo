drag and drop
wantsDropElements: aCollection type: type index: rowIndex
	self dropItemSelector ifNil: [ ^ false ].
	self wantsDropSelector ifNil: [ ^ true ].
	^ self model 
		perform: self wantsDropSelector 
		with: aCollection