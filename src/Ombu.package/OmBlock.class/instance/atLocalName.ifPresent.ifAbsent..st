accessing
atLocalName: aString ifPresent: presentBlockClosure ifAbsent: absentBlockClosure

	| index |
	self checkIfMustRefreshBlock.

	index := aString asInteger - self firstLocalNameAsInteger + 1.
	
	^ index > positions size
		ifTrue: absentBlockClosure
		ifFalse: [
			presentBlockClosure value: (positions at: index) ]