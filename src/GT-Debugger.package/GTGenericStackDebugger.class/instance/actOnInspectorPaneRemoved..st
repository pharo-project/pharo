callbacks
actOnInspectorPaneRemoved: ann 

	ann browser panes size == 1 ifTrue: [ 
		self selectedVariableName: nil ]