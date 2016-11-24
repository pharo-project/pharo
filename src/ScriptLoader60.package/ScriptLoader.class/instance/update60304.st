pharo - updates
update60304
	"self new update60304"
	self withUpdateLog: '19387 DateModelTest leaves rogue window
	https://pharo.fogbugz.com/f/cases/19387

19390 FuelPlatform package should not be there
	https://pharo.fogbugz.com/f/cases/19390

19374 isProvidedByExtension method for MethodAnnouncement
	https://pharo.fogbugz.com/f/cases/19374'.
	self loadTogether: self script60304 merge: false.
	
ScriptLoader new unloadPackage: #FuelPlatform.
	self flushCaches.
