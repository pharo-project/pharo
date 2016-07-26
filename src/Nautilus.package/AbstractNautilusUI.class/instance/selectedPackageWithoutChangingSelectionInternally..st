accessing
selectedPackageWithoutChangingSelectionInternally: aPackage
	| class |
	class := self selectedClass.
	(self showHierarchy not and: [ class notNil and: [ class package ~= aPackage ] ])
		ifTrue: [ self selectedClass: nil ].
	self packageWidget selectPackage: aPackage.
	aPackage ifNotNil: [ self giveFocusTo: self packageWidget ].
	self updateClassView.
	self updateCategoryAndMethodList