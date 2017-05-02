pharo - updates
update60477
	"self new update60477"
	self withUpdateLog: '19909 Versionner reset modifications of first opened project when user opens another project in addition
	https://pharo.fogbugz.com/f/cases/19909

19981 Class side slot initialization does not work
	https://pharo.fogbugz.com/f/cases/19981

20004 SnapshotDone should be after all startup actions
	https://pharo.fogbugz.com/f/cases/20004'.
	self loadTogether: self script60477 merge: false.
	self loadConfiguration: 'Versionner' version: '2.15'.self flushCaches.
