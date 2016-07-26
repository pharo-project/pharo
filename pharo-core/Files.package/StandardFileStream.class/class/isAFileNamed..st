file creation
isAFileNamed: fileName
	"Answer true if a file of the given name exists."

	| f |
	f := self new open: fileName forWrite: false.
	f ifNil: [^ false].
	f close.
	^ true
