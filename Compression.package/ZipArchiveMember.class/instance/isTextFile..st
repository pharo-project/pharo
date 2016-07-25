testing
isTextFile: aBoolean
	"Set whether I am a text file.
	Note that this module does not currently do anything with this flag
	upon extraction or storage.
	That is, bytes are stored in native format whether or not they came
	from a text file."
	internalFileAttributes := aBoolean
		ifTrue: [ internalFileAttributes bitOr: 1 ]
		ifFalse: [ internalFileAttributes bitAnd: 1 bitInvert ]
