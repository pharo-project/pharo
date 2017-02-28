pharo - updates
update60417
	"self new update60417"
	self withUpdateLog: '19770 FFIExternalStructurePlatformTests>>#testStructureHasCorrectOffsets32bits is failing on the bootstrapped image
	https://pharo.fogbugz.com/f/cases/19770

19761 Debugger wrong temp var value after multiple step through block
	https://pharo.fogbugz.com/f/cases/19761'.
	self loadTogether: self script60417 merge: false.
	self flushCaches.
