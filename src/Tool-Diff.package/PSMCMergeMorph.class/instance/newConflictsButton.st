instance-creation
newConflictsButton
	"Answer a new button for displaying the count
	of outstanding conflicts and navigating to each.
	It will be disabled if all conflicts are resolved."

	^(self
		newButtonFor: self
		getState: nil
		action:  #selectNextConflict
		arguments: #()
		getEnabled: #notAllConflictsResolved
		getLabel:  #conflictCountString
		help: 'Select the next conflict in the tree' translated)
		vResizing: #spaceFill "workaround until table  layout fixed"