adding
addRepositoryGroup: aRepositoryGroup
	aRepositoryGroup repositoriesDo: [ :each | 
		each addTo: self ]