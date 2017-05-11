menu
menuActionsForSelectedItems

	| selectedReferences |
	selectedReferences := self selectedEntryReferences.

	selectedReferences isEmpty ifTrue: [ ^#() ].
	
	selectedReferences size = 1 ifTrue: [
		| event |
		event := (self itemForEntryReference: selectedReferences anyOne) event.
		(event isEpRefactoring and: [ event canBuildRBRefactoring ])
			ifTrue: [ ^ self refactoringMenuActions, self eventsMenuActions ]].

	^ self codeChangesMenuActions, self eventsMenuActions
