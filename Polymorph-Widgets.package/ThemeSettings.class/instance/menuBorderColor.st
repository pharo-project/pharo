menu
menuBorderColor
	"Answer the value of menuColor"

	^ self flatMenu  
		ifFalse: [#raised]
		ifTrue: [self derivedMenuColor twiceDarker]