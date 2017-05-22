pharo - updates
update60493
	"self new update60493"
	FFIMethodRegistry resetAll
.
	self withUpdateLog: '20034 case 20011 was not integrated correctly
	https://pharo.fogbugz.com/f/cases/20034

'.
	self loadTogether: self script60493 merge: false.
	self flushCaches.
self cleanRepositories.
