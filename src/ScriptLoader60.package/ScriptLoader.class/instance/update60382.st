pharo - updates
update60382
	"self new update60382"
	self withUpdateLog: 'FileExists error in certain cases due to Epiciea vs new UUID'.
	self loadTogether: self script60382 merge: false.
	self flushCaches.
self cleanRepositories.
