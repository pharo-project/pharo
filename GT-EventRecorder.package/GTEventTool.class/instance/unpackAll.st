api
unpackAll
	"self default unpackAll inspect"
	self ensureLocalDirectory.
	^ GTEventUnpacking default safeUnpackDirectory: self defaultEventsDirectory.