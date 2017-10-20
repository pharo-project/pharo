building widgets
createNotifierPaneWidgets

	self instantiatePresenters: self buildNotifierPaneWidgetsSpec.
	(self widgets keys includes: #codePane) ifTrue: [ 
		(self widgets at: #codePane) beForText.
	].
	self widgets keysDo: [ :key | 
		(self perform: ('initialize', key asString capitalized) asSymbol) ]