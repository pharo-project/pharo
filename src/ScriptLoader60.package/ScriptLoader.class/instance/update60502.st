pharo - updates
update60502
	"self new update60502"
	self withUpdateLog: '20110 AllocationTest>>#testOutOfMemorySignal not well suited to 64-bit
	https://pharo.fogbugz.com/f/cases/20110

20096 Add script pragma to SpaceTally>>printSpaceAnalysis
	https://pharo.fogbugz.com/f/cases/20096

20174  In the debug halo of a morph: clicking on "inspect morph" and "explore morph" bring up the same window.
	https://pharo.fogbugz.com/f/cases/20174'.
	self loadTogether: self script60502 merge: false.
	self flushCaches.
