pharo - updates
update60349
	"self new update60349"
	self withUpdateLog: '19570 QA 3.3.0
	https://pharo.fogbugz.com/f/cases/19570'.
	self loadTogether: self script60349 merge: false.
	self loadConfiguration: 'FlatQA' version: 'v3.3.0'.self flushCaches.
