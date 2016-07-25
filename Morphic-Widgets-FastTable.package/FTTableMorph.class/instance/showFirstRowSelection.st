accessing
showFirstRowSelection
	self hasSelection ifFalse: [ ^ self ].
	self moveShowIndexTo: self selectedRowIndex