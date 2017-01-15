pharo - updates
update60344
	"self new update60344"
	self withUpdateLog: '19509 controling class assignment
	https://pharo.fogbugz.com/f/cases/19509

19515 complileSilently:classified: method does not set properly the package
	https://pharo.fogbugz.com/f/cases/19515

19217 Add printOn: method on WeakAnnouncementSubscription to improve readibility
	https://pharo.fogbugz.com/f/cases/19217

19540 TabMorph should refresh content in background by defer message 
	https://pharo.fogbugz.com/f/cases/19540'.
	self loadTogether: self script60344 merge: false.
	self flushCaches.
