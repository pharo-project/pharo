testing
compareWorkingCopyNamed: wcName using: comarisonOperator
	| fileRef wcRef |
	fileRef := GoferResolvedReference name: self file.
	wcRef := GoferResolvedReference name: wcName.
	^ wcRef compare: fileRef using: comarisonOperator