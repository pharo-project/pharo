private
loadProjects
	^ [ self retrieveProjects collect: [ :each | CatalogProject fromDictionary: each ] ]
		on: ConnectionTimedOut do: [ :e | nil ] 