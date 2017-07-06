pharo - updates
update60507
	"self new update60507"
	self withUpdateLog: '20218 Master branch (Pharo 6) needs to be safely merged into development branch (Pharo 7).
	https://pharo.fogbugz.com/f/cases/20218'.
	self loadTogether: self script60507 merge: false.
	self flushCaches.
