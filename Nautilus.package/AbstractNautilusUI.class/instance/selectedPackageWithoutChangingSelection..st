accessing
selectedPackageWithoutChangingSelection: aPackage
	self okToChange
		ifTrue:
			[ | class |
			class := self selectedClass.
			(self showHierarchy not and: [ class notNil and: [ class package ~= aPackage ] ])
				ifTrue: [ self selectedClass: nil ].
			self model
				package: aPackage
				class: self selectedClass.
			self packageWidget selectPackage: aPackage.
			self updateClassView.
			self updateCategoryAndMethodList ]