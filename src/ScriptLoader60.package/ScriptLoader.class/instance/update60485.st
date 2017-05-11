pharo - updates
update60485
	"self new update60485"
	
EpMonitor reset.
	self withUpdateLog: '20047 in Versioner (or Komitter), RemotesManager needs to list just remotes
	https://pharo.fogbugz.com/f/cases/20047

20005  Epicea: Integrate release 8.2.2
	https://pharo.fogbugz.com/f/cases/20005'.
	self loadTogether: self script60485 merge: false.
	self loadConfiguration: 'Epicea' version: '8.2.2'.
OmStoreFactory reset.
EpMonitor current enable.
EpLostChangesDetector enabled: false.
	self flushCaches.
