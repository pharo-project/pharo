pharo - updates
update60501
	"self new update60501"
	self withUpdateLog: '20093 Missing source stamp makes changes hard to view
	https://pharo.fogbugz.com/f/cases/20093

20148 transforming deprecations should take #showWarning into account
	https://pharo.fogbugz.com/f/cases/20148'.
	self loadTogether: self script60501 merge: false.
	self flushCaches.
