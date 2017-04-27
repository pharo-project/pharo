pharo - updates
update60471
	"self new update60471"
	
EpMonitor reset.
	self withUpdateLog: '19912 Epicea: Integrate release 8.2.1
	https://pharo.fogbugz.com/f/cases/19912'.
	self loadTogether: self script60471 merge: false.
	self loadConfiguration: 'Epicea' version: '8.2.1'.
(RPackageOrganizer default packageNamed: #EpiceaBrowsers) methods do: #recompile.
OmStoreFactory reset.
EpMonitor current enable.
	self flushCaches.
