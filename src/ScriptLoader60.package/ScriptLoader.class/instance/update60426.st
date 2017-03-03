pharo - updates
update60426
	"self new update60426"
	self withUpdateLog: '19788 sometimes external structures does not initialise properly OFFSET fields before use
	https://pharo.fogbugz.com/f/cases/19788

19221 Rub Find And Replace can not search for "?"
	https://pharo.fogbugz.com/f/cases/19221

19789 CI tests often fail on Windows
	https://pharo.fogbugz.com/f/cases/19789'.
	self loadTogether: self script60426 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.26.2'.self flushCaches.
