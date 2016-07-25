private dispatch
renameClassCategory: packageTag
	| newName oldName |

	oldName := packageTag name.
	newName := UIManager default 
		request: ('New name of category {1}' format: { packageTag name }) 
		initialAnswer: oldName
		title: 'Rename a category'.
	newName = oldName ifTrue: [ ^ self ].
	
	(self isValidPackageName: newName)
		ifFalse: [ ^ self alertInvalidPackageName:newName ].		
	(self category:newName existsIn:packageTag package)
		ifTrue:[ ^  self alertCategoryName:newName exitstsAlreadyIn:packageTag package].
	packageTag renameTo: newName.

