accessing
selectedPackageWithoutChangingSelection: aSelection
	| class |
	self okToChange
		ifFalse: [ ^ self ].
	class := self selectedClass.
	(class notNil and: [ aSelection isNil or: [ (self selectedClass: class belongsToPackageSelection: aSelection) not ] ])
		ifTrue: [ self selectedClass: nil ].
	aSelection ifNotNil: [ aSelection registerSelectionTarget: self ].
	self changed: #browseScopedButtonEnabled.
	self updateClassView.
	self updateCategoryAndMethodList.
	self commentTextModel setInitialText: self getComments.