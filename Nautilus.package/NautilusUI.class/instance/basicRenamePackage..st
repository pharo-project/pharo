private
basicRenamePackage: aPackage 

	| newName |
	newName := UIManager default 
		request: 'New name of the package' 
		initialAnswer: aPackage name 
		title: 'Rename a package'.
	newName = aPackage name ifTrue: [ ^ self ].
	
	(self isValidPackageName: newName)
		ifTrue: [ aPackage renameTo: newName ]
		ifFalse: [ self alertInvalidPackageName:newName ].