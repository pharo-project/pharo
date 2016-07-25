public
assureOpen
	"compatibility method to make the FileHandle Tests pass"
	self isOpen ifFalse: [ id := self basicOpen ].