pharo - updates
update60328
	"self new update60328"
	self withUpdateLog: '19472 QA v3.2.10
	https://pharo.fogbugz.com/f/cases/19472

19433 Display critiques in Nautilus inline
	https://pharo.fogbugz.com/f/cases/19433'.
	self loadTogether: self script60328 merge: false.
	self loadConfiguration: 'FlatQA' version: 'v3.2.10'.self flushCaches.
