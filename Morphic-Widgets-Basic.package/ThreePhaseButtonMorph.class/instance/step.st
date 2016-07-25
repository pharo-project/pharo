stepping and presenter
step
	(self hasProperty: #doesButtonAction) ifTrue:[
		self doButtonAction.
		self setProperty: #didButtonAction toValue: true.
	].