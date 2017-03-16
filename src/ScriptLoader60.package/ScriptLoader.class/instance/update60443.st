pharo - updates
update60443
	"self new update60443"
	
EpMonitor reset.
	self withUpdateLog: '19837 Epicea: integrate release 8.1.3
	https://pharo.fogbugz.com/f/cases/19837'.
	self loadTogether: self script60443 merge: false.
	self loadConfiguration: 'Epicea' version: '8.1.3'.
EpMonitor current enable.
	self flushCaches.
