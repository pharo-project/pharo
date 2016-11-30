pharo - updates
update60314
	"self new update60314"
	self withUpdateLog: '19401 New method isExtension for CompiledMethod
	https://pharo.fogbugz.com/f/cases/19401

19344 No event about auto deletion of empty protocol when last method is moved from it
	https://pharo.fogbugz.com/f/cases/19344

19276 Endless loop with ExecutionDisplayPlugin
	https://pharo.fogbugz.com/f/cases/19276'.
	self loadTogether: self script60314 merge: false.
	self flushCaches.
