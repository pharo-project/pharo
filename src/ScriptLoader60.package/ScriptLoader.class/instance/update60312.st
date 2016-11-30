pharo - updates
update60312
	"self new update60312"
	self withUpdateLog: '19407 FastTable default intercell spacing for glamour is too big
	https://pharo.fogbugz.com/f/cases/19407

19408 Custom method icon for FFI methods
	https://pharo.fogbugz.com/f/cases/19408

'.
	self loadTogether: self script60312 merge: false.
	self flushCaches.
self cleanRepositories.
