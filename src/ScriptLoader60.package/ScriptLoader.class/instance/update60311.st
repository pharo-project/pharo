pharo - updates
update60311
	"self new update60311"
	self withUpdateLog: '19408 Custom method icon for FFI methods
	https://pharo.fogbugz.com/f/cases/19408

19407 FastTable default intercell spacing for glamour is too big
	https://pharo.fogbugz.com/f/cases/19407

19401 New method isExtension for CompiledMethod
	https://pharo.fogbugz.com/f/cases/19401

19411 improve comment RBGlobalNode for class variable
	https://pharo.fogbugz.com/f/cases/19411

19410 improve comment  OCRequestorScope
	https://pharo.fogbugz.com/f/cases/19410'.
	self loadTogether: self script60311 merge: false.
	self flushCaches.
