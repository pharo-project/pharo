pharo - updates
update60500
	"self new update60500"
	self withUpdateLog: '20146 ZnHTTPSTests>>#testGetPharoVersion started to fail
	https://pharo.fogbugz.com/f/cases/20146'.
	self loadTogether: self script60500 merge: false.
	self flushCaches.
