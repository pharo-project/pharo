pharo - updates
update60463
	"self new update60463"
	self withUpdateLog: '19923 QA v3.3.1
	https://pharo.fogbugz.com/f/cases/19923

19922 System-BasicCommandLineHandler calls name method
	https://pharo.fogbugz.com/f/cases/19922'.
	self loadTogether: self script60463 merge: false.
	self loadConfiguration: 'FlatQA' version: 'v3.3.1'.self flushCaches.
