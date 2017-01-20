pharo - updates
update60345
	"self new update60345"
	self withUpdateLog: '19548 Fix Spec Integration class comments and examples
	https://pharo.fogbugz.com/f/cases/19548

19545 Add comment about OC translator subclasses (for effect / for value)
	https://pharo.fogbugz.com/f/cases/19545

19555 Nautilus should sort all binary methods before non-binary methods
	https://pharo.fogbugz.com/f/cases/19555

19550 Typo in settings: Popup notifaction --> Popup notification
	https://pharo.fogbugz.com/f/cases/19550'.
	self loadTogether: self script60345 merge: false.
	self flushCaches.
