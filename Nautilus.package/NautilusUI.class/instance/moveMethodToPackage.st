menus behavior
moveMethodToPackage
	"Search for a package from a pattern or from the recent list"	
	
	| foundPackage  |	
	self okToChange ifFalse: [^ self flashPackage ].
	foundPackage := SearchFacade rPackageSearch chooseFromOwner: self window.
	foundPackage ifNil: [ ^self ].
	self selectedMethods do: [ :each | self moveMethod: each inPackage: foundPackage ].
	self updateCategoryAndMethodList.
		
