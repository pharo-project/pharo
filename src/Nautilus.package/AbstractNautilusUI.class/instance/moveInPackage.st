menus behavior
moveInPackage

	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"
	| foundPackage |
	self okToChange ifFalse: [^ self].
	foundPackage := SearchFacade rPackageSearch chooseFromOwner: self window.
	foundPackage ifNil: [ ^self ].
	self selectedClasses 
		do: [:aClass | aClass category: foundPackage name ].
		self flag: #esteban. "unclear that we should still use category:"
	 self selectedPackage: foundPackage.
	self updatePackageGroupAndClassList