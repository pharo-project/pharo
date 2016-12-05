pharo - updates
update60316
	"self new update60316"
	self withUpdateLog: '19418 Spec TextModel editor has broken cancel operation after first accept
	https://pharo.fogbugz.com/f/cases/19418

19413 GTInspector/Debugger do not work for large/ toolarge intervals
	https://pharo.fogbugz.com/f/cases/19413

19420 redu menu item is wrong and not working for rubric
	https://pharo.fogbugz.com/f/cases/19420'.
	self loadTogether: self script60316 merge: false.
	self flushCaches.
