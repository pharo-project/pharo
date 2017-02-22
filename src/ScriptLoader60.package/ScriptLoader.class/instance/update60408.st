pharo - updates
update60408
	"self new update60408"
	self withUpdateLog: '19743 Fix Refactoring-Tests-Group in BaselineOfIDE
	https://pharo.fogbugz.com/f/cases/19743

19744 integrate PharoBootstrap-Initialization
	https://pharo.fogbugz.com/f/cases/19744'.
	self loadTogether: self script60408 merge: false.
	self flushCaches.
