accessing
selectedClass: aClass
	self okToChangeSourceAndComment
		ifFalse: [ ^ self ].
	self resetClassesListSelection.
	aClass ifNotNil: [ self giveFocusTo: self classWidget ].
	self selectedClassWithoutChangingSelection: aClass