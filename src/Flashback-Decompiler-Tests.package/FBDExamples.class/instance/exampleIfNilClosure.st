examples - branches
exampleIfNilClosure
	^ iVar ifNil: [ 1 + ({1.2} at: 2 ifAbsent: [ ^ nil ]) ]