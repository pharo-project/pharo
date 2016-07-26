testing
compareWorkingCopyNamed: wcName using: comarisonOperator
	| fileRef wcRef |
	fileRef := GoferResolvedReference name: self spec file.
	wcRef := GoferResolvedReference name: wcName.
	^ wcRef compare: fileRef using: comarisonOperator