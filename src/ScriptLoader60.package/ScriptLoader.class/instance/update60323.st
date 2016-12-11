pharo - updates
update60323
	"self new update60323"
	self withUpdateLog: '19449 QA v3.2.9 update
	https://pharo.fogbugz.com/f/cases/19449

19440 No users of TotalCost and subclasses
	https://pharo.fogbugz.com/f/cases/19440'.
	self loadTogether: self script60323 merge: false.
	self loadConfiguration: 'FlatQA' version: 'v3.2.9'.self flushCaches.
