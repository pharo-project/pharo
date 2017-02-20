pharo - updates
update60404
	"self new update60404"
	self withUpdateLog: '18915 Kernel depends on Reflectivity
	https://pharo.fogbugz.com/f/cases/18915

19654 Tooltips with description of the over and through debugger buttons actions
	https://pharo.fogbugz.com/f/cases/19654

19724 Inspecting socket can produce PrimitiveFailed
	https://pharo.fogbugz.com/f/cases/19724'.
	self loadTogether: self script60404 merge: false.
	self flushCaches.
