private
loadProjects
	^ self httpClient get 
		collect: [ :each | CatalogProject fromDictionary: each ]