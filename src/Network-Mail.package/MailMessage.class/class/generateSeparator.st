utilities
generateSeparator
	"generate a separator usable for making MIME multipart documents.  A leading -- will *not* be included"
	^'==CelesteAttachment' , (10000 to: 99999) atRandom asString , '=='.