updating
updateFromImage
	self configuration updateFromImage.
	self changed: #dependencyList; changed: #description
