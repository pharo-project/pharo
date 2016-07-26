private
renamePackage: anRPackage

	| newName |
	newName := UIManager default 
		request: 'New name of the package' 
		initialAnswer: anRPackage name 
		title: 'Rename a package'.
	newName = anRPackage name ifTrue: [ ^ self ].
	(self isValidPackageName: newName)
		ifTrue: [ anRPackage renameTo: newName ]
		ifFalse: [ self alertInvalidPackageName:newName ].