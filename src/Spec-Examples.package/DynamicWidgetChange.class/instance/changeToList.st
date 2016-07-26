private
changeToList

	self instantiateModels: 
		#( bottom #ListModel ).

	button font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10).
		
	bottom items: (1 to: 100) asOrderedCollection.