menus behavior
findMethod
	"Search for a method in the lookup of the selected class"

	| foundMethod class |
	self okToChange
		ifFalse: [ ^ self flashPackage ].
	class := self selectedClass.
	class ifNil: [ ^ self ].
	foundMethod := (SearchFacade messageSearchFor: class)
		chooseFromOwner: self window.
	foundMethod ifNil: [ ^ self ].
	self resetCategoriesListSelection.
	self resetMethodsListSelection.
	self categoryWidget selectProtocol: foundMethod category.
	self model
		package: self selectedPackage
		class: self selectedClass
		protocol: foundMethod protocol
		method: foundMethod.
	self updateCategoryAndMethodList