utils
changeNameOf: exportedClass to: aNewName
	(exportedClass isKindOf: HETrait)
		ifTrue: [ 
				exportedClass methods do:[:e | self renameExportedClass: exportedClass traitName to: aNewName on:e ].
				exportedClass instVarNamed: #traitName put: aNewName. 
			]
		ifFalse: [ 
			exportedClass methods do:[:e | self renameExportedClass: exportedClass className to: aNewName on:e ].			
			exportedClass instVarNamed: #className put: aNewName ].
		
