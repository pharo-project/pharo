pharo - updates
update60389
	"self new update60389"
	self withUpdateLog: '19701 Watchpoint should not ask for name
	https://pharo.fogbugz.com/f/cases/19701

19700 testTimeStamp should not use hardcoded timestamp
	https://pharo.fogbugz.com/f/cases/19700

19699 testProtocol should not test timestamp
	https://pharo.fogbugz.com/f/cases/19699'.
	self loadTogether: self script60389 merge: false.
	self flushCaches.
