pharo - updates
update60306
	"self new update60306"
	self withUpdateLog: '19365 remove dependency of Catalog on Nautilus
	https://pharo.fogbugz.com/f/cases/19365

19367 testClassRespectsPolymorphismWithTrait does suppose full image
	https://pharo.fogbugz.com/f/cases/19367'.
	self loadTogether: self script60306 merge: false.
	self loadConfiguration: 'Catalog' version: '0.17'.self flushCaches.
