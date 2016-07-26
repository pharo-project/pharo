private
selectionChangedUpdateIndex: ann
	self setIndexSelector ifNil: [ ^ self ].

	self model
		perform: self setIndexSelector
		with: (ann newSelectedRowIndexes 
			ifNotEmpty: [ :indexes | indexes first ]
			ifEmpty: [ 0 ])