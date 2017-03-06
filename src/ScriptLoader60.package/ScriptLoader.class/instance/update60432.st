pharo - updates
update60432
	"self new update60432"
	self withUpdateLog: '19808 add timeout and cache per session to catalog
	https://pharo.fogbugz.com/f/cases/19808

19803 MCSTWriterTest>>#testMethodDefinitionWithBangs failing on bootstrapped image
	https://pharo.fogbugz.com/f/cases/19803

19811 some methods are wrong in DynamicLoader
	https://pharo.fogbugz.com/f/cases/19811'.
	self loadTogether: self script60432 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.26.5'.self flushCaches.
