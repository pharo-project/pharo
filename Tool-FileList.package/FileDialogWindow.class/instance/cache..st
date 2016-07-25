accessing
cache: dir
	"Cache the contents of the given directory and answer them."

	self entryCacheDirectory = dir
		ifFalse: [Cursor wait showWhile: [
					self
						entryCache: (self dirEntriesOrEmptyListFor: dir);
						entryCacheDirectory: dir]].
	^self entryCache