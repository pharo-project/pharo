pharo - updates
update60321
	"self new update60321"
	self withUpdateLog: '19437 Do not log shadowed variables to transcript by default
	https://pharo.fogbugz.com/f/cases/19437

19450 Suggestion menu not working on MessageBrowser 
	https://pharo.fogbugz.com/f/cases/19450

19441 Better description for MessageNotUnderstood
	https://pharo.fogbugz.com/f/cases/19441'.
	self loadTogether: self script60321 merge: false.
	self flushCaches.
