pharo - updates
update60308
	"self new update60308"
	self withUpdateLog: '19276 Endless loop with ExecutionDisplayPlugin
	https://pharo.fogbugz.com/f/cases/19276

19344 No event about auto deletion of empty protocol when last method is moved from it
	https://pharo.fogbugz.com/f/cases/19344

17771 Empty stderr/stdout files should be deleted on image shutdown
	https://pharo.fogbugz.com/f/cases/17771'.
	self loadTogether: self script60308 merge: false.
	self flushCaches.
