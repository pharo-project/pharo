pharo - updates
update60320
	"self new update60320"
	self withUpdateLog: '19442 PharoDocComment
	https://pharo.fogbugz.com/f/cases/19442

19443 ProtocolAnalyser
	https://pharo.fogbugz.com/f/cases/19443'.
	self loadTogether: self script60320 merge: false.
	self loadConfiguration: 'PharoDocComment' version: '1.0'.
self loadConfiguration: 'ProtocolAnalyzer' version: '1.0'.self flushCaches.
