instance creation
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	^ s nextWordsInto: (self new: 6)