pharo - updates
update60332
	"self new update60332"
	self withUpdateLog: '19480 Update Glamour to version 4.27
	https://pharo.fogbugz.com/f/cases/19480'.
	self loadTogether: self script60332 merge: false.
	self loadConfiguration: 'GlamourCore' version: '4.27'.self flushCaches.
