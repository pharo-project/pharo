refactoring
selectTheTextForTheNode
	selectionPreviousHighligth := self selectedInterval.
	selectedNode ifNotNil: [ self contentSelection ]