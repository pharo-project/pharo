pharo - updates
update60462
	"self new update60462"
	self withUpdateLog: '19921 allow CairoLibrary to find x86_64 bits libcairo when needed (linux)
	https://pharo.fogbugz.com/f/cases/19921'.
	self loadTogether: self script60462 merge: false.
	self flushCaches.
