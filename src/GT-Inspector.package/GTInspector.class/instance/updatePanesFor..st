actions
updatePanesFor: anObject

	(self panes select: [ :each | 
		(each port: #entity) value == anObject ]) do: [ :each | each update ]