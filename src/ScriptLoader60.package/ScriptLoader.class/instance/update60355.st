pharo - updates
update60355
	"self new update60355"
	self withUpdateLog: '19593 TClassDescription>>#isTestCase should be in SUnit-Core
	https://pharo.fogbugz.com/f/cases/19593

19558 GtDebugger should not call #updateSelectionInterval in #updateBrowser
	https://pharo.fogbugz.com/f/cases/19558'.
	self loadTogether: self script60355 merge: false.
	self flushCaches.
