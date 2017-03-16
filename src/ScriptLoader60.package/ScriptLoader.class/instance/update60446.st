pharo - updates
update60446
	"self new update60446"
	self withUpdateLog: '19842 add simple api for testing if a link is installed in a node
	https://pharo.fogbugz.com/f/cases/19842

19841 Format as you read can produce confusing "contents has been modified" questions
	https://pharo.fogbugz.com/f/cases/19841

19843 add simple demo of logging self sends
	https://pharo.fogbugz.com/f/cases/19843'.
	self loadTogether: self script60446 merge: false.
	self flushCaches.
