morphic ui
performButtonAction: anActionSelector enabled: anEnabledSelector
	(self perform: anEnabledSelector) 
		ifTrue: [ self perform: anActionSelector ]