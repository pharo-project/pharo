pharo - updates
update60348
	"self new update60348"
	self withUpdateLog: '19557 GTDebugger should use FastTable in the object inspector for thisContext
	https://pharo.fogbugz.com/f/cases/19557

19561 Drag&Drop FastTable Example Does Not Work
	https://pharo.fogbugz.com/f/cases/19561

19536 "Instance variables not read and written" critique false positive on classes with certain slots
	https://pharo.fogbugz.com/f/cases/19536

19559 GTMoldableDebugger>>updateBrowser should not call #update
	https://pharo.fogbugz.com/f/cases/19559'.
	self loadTogether: self script60348 merge: false.
	self flushCaches.
