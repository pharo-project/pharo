system integration
systemCategoryRemovedActionFrom: ann
	"When a system category is removed, we may: remove a tag, or remove a rpackage. If we remove a RPackage, unregister the linked MCWorkingCopy. If it is a tag, do nothing? (from what I know of RPackage, the tag should already have disappeared because it would have been empty)."

	| rPackage categoryName categoryNameSymbol managers |
	categoryName := ann categoryName.
	categoryNameSymbol := categoryName asSymbol.
	self flag: #hack.	"for decoupling MC"
	managers := (self respondsTo: #allManagers)
		ifTrue: [ self perform: #allManagers ]
		ifFalse: [ #() ].	
	"Reverse the test. First check that this is the root category of a RPackage. If yes, unregister the RPackage and the corresponding MCWorkingCopy."
	rPackage := packages at: categoryNameSymbol ifAbsent: [ ^ self ].
	"Consider that a rPackage with extension selectors or tags is not empty and shouldn't be removed."
	(rPackage extensionSelectors notEmpty
		or: [ (rPackage classTags reject: [ :tag | tag name = categoryName ]) notEmpty ])
		ifTrue: [ ^ self ].
	self basicUnregisterPackageNamed: categoryNameSymbol.
	(managers detect: [ :each | each packageName = categoryName ] ifNone: [  ])
		ifNotNil: [ :mcWorkingCopy | mcWorkingCopy unregister ]