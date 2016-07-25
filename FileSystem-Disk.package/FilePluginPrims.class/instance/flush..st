file primitives
flush: id
	"Flush pending changes to the disk"
	
	| pos |
	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
	"In some OS's seeking to 0 and back will do a flush"
	pos := self getPosition: id.
	self setPosition: id to: 0; setPosition: id to: pos