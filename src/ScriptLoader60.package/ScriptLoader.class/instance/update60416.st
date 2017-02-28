pharo - updates
update60416
	"self new update60416"
	self withUpdateLog: '19732 Reenable Catalog loading in Spotter and fix confusing/hidden setting for it
	https://pharo.fogbugz.com/f/cases/19732

19769 BaselineOfIDE needs update
	https://pharo.fogbugz.com/f/cases/19769

19771 remove debugging "Alive" report during the bootstrap initialization
	https://pharo.fogbugz.com/f/cases/19771'.
	self loadTogether: self script60416 merge: false.
	
(GTSpotterExtensionSettings perform: #GTSpotter_spotterCatalogProjectsFor: with: true).
 (CatalogSettings displayCatalogProjectsInSpotter: false).
	self flushCaches.
