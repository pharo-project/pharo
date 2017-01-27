pharo - updates
update60360
	"self new update60360"
	self withUpdateLog: '19568 Socket local host/port has incorrect guard
	https://pharo.fogbugz.com/f/cases/19568'.
	self loadTogether: self script60360 merge: false.
	self flushCaches.
