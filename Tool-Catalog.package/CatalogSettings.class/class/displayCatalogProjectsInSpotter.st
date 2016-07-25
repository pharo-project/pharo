accessing
displayCatalogProjectsInSpotter
	"Return true if the catalog projects should be displayed in spotter, false otherwise"
	
	DisplayCatalogProjectsInSpotter ifNil: [ DisplayCatalogProjectsInSpotter := true ].
	^DisplayCatalogProjectsInSpotter