pharo - updates
update60481
	"self new update60481"
	self withUpdateLog: '19984 TabExample does not switch theme correctly
	https://pharo.fogbugz.com/f/cases/19984

20014 Dark theme is bad for changes browser
	https://pharo.fogbugz.com/f/cases/20014

19975 Can add a _tag_ to a Nautilus work group but a package is expected
	https://pharo.fogbugz.com/f/cases/19975

20016 Extensions with the same prefix may not be categorized correctly
	https://pharo.fogbugz.com/f/cases/20016'.
	self loadTogether: self script60481 merge: false.
	self flushCaches.
