pharo - updates
update60395
	"self new update60395"
	self withUpdateLog: '19708 When test took too much time while forked processes was failed then TestFailedByForkedProcesses should be signalled
	https://pharo.fogbugz.com/f/cases/19708

19714 Force migration of instances of EphemeronLayout and ImmediateLayout
	https://pharo.fogbugz.com/f/cases/19714'.
	self loadTogether: self script60395 merge: false.
	
EphemeronLayout addInstVarNamed: 'toRemove'.
EphemeronLayout removeInstVarNamed:  'toRemove'.
ImmediateLayout addInstVarNamed: 'toRemove'.
ImmediateLayout removeInstVarNamed:  'toRemove'.
	self flushCaches.
