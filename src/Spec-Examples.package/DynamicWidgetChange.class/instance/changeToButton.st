private
changeToButton

	self instantiatePresenters: 
		#( bottom ButtonPresenter ).
	button font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 9).
		
	bottom label: 'I am useless'.