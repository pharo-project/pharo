accessing
lastLocalNameAsInteger

	self checkIfMustRefreshBlock.
	
	^ positions size + self firstLocalNameAsInteger - 1.
