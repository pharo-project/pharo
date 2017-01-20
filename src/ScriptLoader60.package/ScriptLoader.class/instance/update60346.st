pharo - updates
update60346
	"self new update60346"
	self withUpdateLog: '19565 QA v3.2.11
	https://pharo.fogbugz.com/f/cases/19565'.
	self loadTogether: self script60346 merge: false.
	self loadConfiguration: 'FlatQA' version: 'v3.2.11'.self flushCaches.
