pharo - updates
update60491
	"self new update60491"
	self withUpdateLog: '20034 case 20011 was not integrated correctly
	https://pharo.fogbugz.com/f/cases/20034

'.
	self loadTogether: self script60491 merge: false.
	FFIMethodRegistry resetAll.
	self flushCaches.
self cleanRepositories.
