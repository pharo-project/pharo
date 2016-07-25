private
verticalScrollBarValue: aNumber

	self hasDataSource ifFalse: [ ^ self ].
	
	showIndex := self verticalScrollBarValueToRowIndex: aNumber.
	
	self container changed