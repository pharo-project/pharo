menus behavior
findProtocol
	"Search for a method in the lookup of the selected class"

	| foundProtocol class |
	class := self selectedClass.
	class ifNil: [ ^ self ].
	self okToChange
		ifFalse: [ ^ self flashPackage ].
	foundProtocol := (SearchFacade protocolSearchFor: class)
		chooseFromOwner: self window.
	foundProtocol ifNil: [ ^ self ].
	self resetCategoriesListSelection.
	self categoryWidget selectProtocol: foundProtocol.
	self model
		package: class package
		class: class
		protocol: foundProtocol.
	self updatePackageGroupAndClassList.
	self updateCategoryAndMethodList