accessing
basenameWithoutExtension: anExtension
	"Returns the basename without specified extension (if any)
	('/foo/gloops.taz' asPath basenameWithoutExtension: 'taz') = 'gloops'
	"
	| extensionWithDelimiter |

	extensionWithDelimiter := anExtension copyWithFirst: self extensionDelimiter.
	(self basename endsWith: extensionWithDelimiter)
		ifTrue: [^ self basename allButLast: extensionWithDelimiter size]
		ifFalse: [ ^ self basename ]