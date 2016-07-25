menus behavior
findPackage

	"Search for a package from a pattern or from the recent list"
	| foundPackage |
	self okToChange ifFalse: [^ self ].	
	foundPackage := SearchFacade rPackageSearch chooseFromOwner: self window.
	foundPackage ifNil: [ ^self ].
	self selectedPackage: foundPackage.
	self updatePackageViewAndMove.
