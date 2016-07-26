dnd requests
requestDropStream: dropIndex
	"Return a read-only stream for some file the user has just dropped onto the application."

	| rawName |
	rawName := self primDropRequestFileName: dropIndex.
	name := FilePathEncoder decode: rawName.
	fileID := self primDropRequestFileHandle: dropIndex.
	fileID == nil
		ifTrue: [ ^ nil ].
	self register.
	rwmode := false.
	buffer1 := String new: 1.
	self enableReadBuffering