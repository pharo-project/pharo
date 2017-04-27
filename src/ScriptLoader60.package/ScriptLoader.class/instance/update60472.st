pharo - updates
update60472
	"self new update60472"
	self withUpdateLog: '19992 Use assert:equals: in simulateKeyStroke tests
	https://pharo.fogbugz.com/f/cases/19992

19463 Improve SessionManager class comment
	https://pharo.fogbugz.com/f/cases/19463

19983 activate the Dark theme by default in the bootstrapped image
	https://pharo.fogbugz.com/f/cases/19983

19991 Use of deprecated name method in TextEditor>>changeStyle
	https://pharo.fogbugz.com/f/cases/19991'.
	self loadTogether: self script60472 merge: false.
	self flushCaches.
