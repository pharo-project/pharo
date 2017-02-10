pharo - updates
update60385
	"self new update60385"
	
EpMonitor current sessionStore class defaultBaseLocator: nil. EpMonitor reset.
	self withUpdateLog: '19670 The tree widget for FastTable jumps to the selected element when expanding or collapsing a node
	https://pharo.fogbugz.com/f/cases/19670

19630 Epicea Lost Changes Detector: MNU OmNullReference>>globalName
	https://pharo.fogbugz.com/f/cases/19630

19674 Remove UUIDGenerator and rename NeoUUIDGenerator to UUIDGenerator
	https://pharo.fogbugz.com/f/cases/19674

19681 #signalerContext should use #method, not #methodNode
	https://pharo.fogbugz.com/f/cases/19681'.
	self loadTogether: self script60385 merge: false.
	self loadConfiguration: 'Epicea' version: '8.1.1'.
EpMonitor current enable. .
	self flushCaches.
