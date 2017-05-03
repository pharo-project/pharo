pharo - updates
update60479
	"self new update60479"
	self withUpdateLog: '20011 OSEnvironment>>getEnv: needs to use a primitive
	https://pharo.fogbugz.com/f/cases/20011

19936 The code presentation in the Meta tab cuts the scrollbar at the bottom
	https://pharo.fogbugz.com/f/cases/19936

19897 Problem with presentations containing tabulators in the inspector from the debugger
	https://pharo.fogbugz.com/f/cases/19897'.
	self loadTogether: self script60479 merge: false.
	self flushCaches.
