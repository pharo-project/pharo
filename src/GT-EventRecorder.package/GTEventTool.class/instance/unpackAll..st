api
unpackAll: aFileReference
	"self default unpackAll: './gt/events' asFileReference.
	self default unpackAll: './gt/events/roassal2Examples' asFileReference.
	" 
	self ensureLocalDirectory.
	^ GTEventUnpacking default safeUnpackDirectory: aFileReference.