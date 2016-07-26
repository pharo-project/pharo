accessing
selectedGroupWithoutChangingSelection: anObject

	self okToChange
		ifTrue: [
			anObject ifNil: [ self packageWidget resetGroupsSelection ].
			self model selectedGroup: anObject.
			self packageWidget selectGroup: anObject.
			self selectedClass: nil.
			self updateClassView ]