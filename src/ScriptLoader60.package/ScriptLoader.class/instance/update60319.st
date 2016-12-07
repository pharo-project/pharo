pharo - updates
update60319
	"self new update60319"
	self withUpdateLog: '19412 SpecDebugger cannot work without Glamour
	https://pharo.fogbugz.com/f/cases/19412

19428 Silently ignore loading DelayScheduler setting when its the same as existing
	https://pharo.fogbugz.com/f/cases/19428

19426 TabsManagerMorph improvements
	https://pharo.fogbugz.com/f/cases/19426'.
	self loadTogether: self script60319 merge: false.
	self flushCaches.
