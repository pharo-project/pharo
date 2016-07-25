selection
selectTheTextForTheNode
	selectionPreviousHighligth := self selectedInterval .
	selectedNode ifNotNil: [ self sourceTextArea selectFrom: self nodeStart to: self nodeStop]