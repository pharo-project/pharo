utility
insertLineFeeds
	"(FileStream oldFileNamed: 'BBfix2.st') insertLineFeeds"
	| s crLf f |
	crLf := String crlf.
	s := self next: self size.
	self close.
	f := FileStream newFileNamed: self name.
	s linesDo: [:line | f nextPutAll: line; nextPutAll: crLf].
	f close