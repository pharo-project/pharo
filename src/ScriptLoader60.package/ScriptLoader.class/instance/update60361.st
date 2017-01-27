pharo - updates
update60361
	"self new update60361"
	self withUpdateLog: '18958 FileReference API enhancement
	https://pharo.fogbugz.com/f/cases/18958

19611 rename asMethodConst into asMethodConstant
	https://pharo.fogbugz.com/f/cases/19611

13159 ListDialogWindow - Grow Width to Fit List
	https://pharo.fogbugz.com/f/cases/13159'.
	self loadTogether: self script60361 merge: false.
	self flushCaches.
