pharo - updates
update60459
	"self new update60459"
	self withUpdateLog: '19907 Improve Comment OpalCompiler
	https://pharo.fogbugz.com/f/cases/19907

19911 Epicea makes a memory leak
	https://pharo.fogbugz.com/f/cases/19911'.
	self loadTogether: self script60459 merge: false.
	
EpMonitor reset. EpMonitor current enable. Smalltalk garbageCollect.
	self flushCaches.
