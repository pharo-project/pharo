pharo - updates
update60341
	"self new update60341"
	self withUpdateLog: '19521 localMethods should be moved to TBehavior where localSelectors methods are defined 
	https://pharo.fogbugz.com/f/cases/19521

19528 Split larger variable entries in the Variables menu into submenus
	https://pharo.fogbugz.com/f/cases/19528'.
	self loadTogether: self script60341 merge: false.
	self flushCaches.
