updating
updateFromRepositories
	self configuration updateFromRepositories.
	self changed: #dependencyList; changed: #description
