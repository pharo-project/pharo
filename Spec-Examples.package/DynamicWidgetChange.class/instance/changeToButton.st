private
changeToButton

	self instantiateModels: 
		#( bottom ButtonModel ).
	button font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 9).
		
	bottom label: 'I am useless'.