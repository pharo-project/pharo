pharo - updates
update60310
	"self new update60310"
	self withUpdateLog: '19407 FastTable default intercell spacing for glamour is too big
	https://pharo.fogbugz.com/f/cases/19407

19401 New method isExtension for CompiledMethod
	https://pharo.fogbugz.com/f/cases/19401

19408 Custom method icon for FFI methods
	https://pharo.fogbugz.com/f/cases/19408'.
	self loadTogether: self script60310 merge: false.
	self flushCaches.
