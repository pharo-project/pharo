private
selectionChanged: ann
	self selectionChangedUpdateStatus: ann.
	self selectionChangedUpdateIndex: ann.
	self selectionChangedResetIfEmpty: ann