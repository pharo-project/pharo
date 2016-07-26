private
dirEntriesOrEmptyListFor: aDirectory
|entries|
	aDirectory isReadable ifFalse:[ ^ #()].
	entries := [aDirectory entries] on: DirectoryDoesNotExist do:[#()].
	^ entries