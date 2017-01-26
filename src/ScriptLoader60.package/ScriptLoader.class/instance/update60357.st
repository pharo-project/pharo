pharo - updates
update60357
	"self new update60357"
	self withUpdateLog: '19601 [Epicea] Epicea monitor is off by default
	https://pharo.fogbugz.com/f/cases/19601

19600 [Epicea] OmNullReference shuld provide #globalName
	https://pharo.fogbugz.com/f/cases/19600'.
	self loadTogether: self script60357 merge: false.
	self loadConfiguration: 'Epicea' version: '8.0.8'.self flushCaches.
