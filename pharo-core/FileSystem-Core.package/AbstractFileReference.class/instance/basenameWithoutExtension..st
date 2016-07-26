accessing
basenameWithoutExtension: anExtension
	"Returns the basename without specified extension (if any)
	('/foo/gloops.taz' asFileReference basenameWithoutExtension: 'taz') = 'gloops'
	"
	^ self fullPath basenameWithoutExtension: anExtension