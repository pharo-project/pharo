pharo - updates
update60437
	"self new update60437"
	self withUpdateLog: '19814 OSPlatform current needs to differentiate 32 and 64 bits
	https://pharo.fogbugz.com/f/cases/19814

19821 loading of the BaselineOfIDE is broken
	https://pharo.fogbugz.com/f/cases/19821'.
	self loadTogether: self script60437 merge: false.
	self flushCaches.
