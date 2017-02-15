pharo - updates
update60392
	"self new update60392"
	self withUpdateLog: '19707 remove Object>>#name hack in the BaselineOfIDE
	https://pharo.fogbugz.com/f/cases/19707

19706 remove MBConfigurationInfoTest>>#testInitialization
	https://pharo.fogbugz.com/f/cases/19706

19704 [Memory leak] Clipboard can produce memory leaks
	https://pharo.fogbugz.com/f/cases/19704'.
	self loadTogether: self script60392 merge: false.
	self flushCaches.
