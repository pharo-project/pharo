selection
selectTheTextForTheNode
	selectionPreviousHighligth := self model primarySelectionInterval .
	selectedNode ifNotNil: [ self model primarySelectionInterval: (self nodeStart to: self nodeStop)]