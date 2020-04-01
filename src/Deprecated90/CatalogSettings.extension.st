Extension { #name : #CatalogSettings }

{ #category : #'*Deprecated90' }
CatalogSettings class >> displayCatalogProjectsInSpotter: aBoolean [
	self deprecated: 'Now use SptCatalogProjectsProcessor #enabled: method'.
	
	SptCatalogProjectsProcessor enabled: aBoolean
	
]
