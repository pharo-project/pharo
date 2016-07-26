renaming
renameClass: aClass as: newName 
	"Rename the class, aClass, to have the title newName."

	^self renameClass: aClass from: aClass name to: newName