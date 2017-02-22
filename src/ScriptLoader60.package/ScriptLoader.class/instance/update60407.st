pharo - updates
update60407
	"self new update60407"
	self withUpdateLog: '19736 Catalog should use Metacello API instead of Gofer for Configuration installation
	https://pharo.fogbugz.com/f/cases/19736

19446 Extract Method is broken
	https://pharo.fogbugz.com/f/cases/19446'.
	self loadTogether: self script60407 merge: false.
	self flushCaches.
