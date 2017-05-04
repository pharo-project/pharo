pharo - updates
update60482
	"self new update60482"
	self withUpdateLog: '20010 Deprecated methods should be placed in the Deprecated60 package
	https://pharo.fogbugz.com/f/cases/20010

20021 fix for #instead: stack needs to be cleaned
	https://pharo.fogbugz.com/f/cases/20021'.
	self loadTogether: self script60482 merge: false.
	self flushCaches.
