private
changeToList

	self instantiateModels: 
		#( bottom #ListPresenter ).

	button font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10).
		
	bottom items: (1 to: 100) asOrderedCollection.