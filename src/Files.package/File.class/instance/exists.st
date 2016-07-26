open/close
exists

	| handle |
	"We open it for read. If the returned handle is nil, the file could not be opened"
	handle := self basicOpenForWrite: false.
	handle ifNil: [^ false].
	self class primClose: handle.
	^ true