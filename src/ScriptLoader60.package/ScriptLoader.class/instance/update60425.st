pharo - updates
update60425
	"self new update60425"
	self withUpdateLog: '19717 FileSystem workingDirectory wrong after image moved to a new folder
	https://pharo.fogbugz.com/f/cases/19717

18578 MetacelloValidationError(MetacelloValidationIssue) printOn: rasies Warning #name
	https://pharo.fogbugz.com/f/cases/18578'.
	self loadTogether: self script60425 merge: false.
	self flushCaches.
