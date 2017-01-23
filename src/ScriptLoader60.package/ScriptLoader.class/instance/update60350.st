pharo - updates
update60350
	"self new update60350"
	self withUpdateLog: '19556 Glamour version 4.31
	https://pharo.fogbugz.com/f/cases/19556'.
	self loadTogether: self script60350 merge: false.
	self loadConfiguration: 'GlamourCore' version: '4.31'.self flushCaches.
