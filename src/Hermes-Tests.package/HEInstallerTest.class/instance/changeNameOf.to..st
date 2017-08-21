utils
changeNameOf: exportedClass to: aNewName
	(exportedClass isKindOf: HETrait)
		ifTrue: [ exportedClass instVarNamed: #traitName put: aNewName ]
		ifFalse: [ exportedClass instVarNamed: #className put: aNewName ]