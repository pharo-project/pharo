accessing
basenameWithoutExtension: anExtension
	"Returns the basename without specified extension (if any)
	('/foo/gloops.taz' asPath basenameWithoutExtension: 'taz') = 'gloops'
	"
	| extensionWihtDelimiter |

	extensionWihtDelimiter := anExtension copyWithFirst: self extensionDelimiter.
	(self basename endsWith: extensionWihtDelimiter)
		ifTrue: [^ self basename allButLast: extensionWihtDelimiter size]
		ifFalse: [ ^ self basename ]