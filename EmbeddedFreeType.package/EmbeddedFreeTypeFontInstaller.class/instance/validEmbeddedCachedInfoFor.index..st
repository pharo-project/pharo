accessing
validEmbeddedCachedInfoFor: bytes index: i
	"answer info from cache if the bytes are the same as the cached info, otherwise answer nil"
	| cacheEntry fileSize |

	fileSize := bytes size.
	cacheEntry := (embeddedFileInfoCache at: {fileSize. i} ifAbsentPut:[Set new])
		detect:[:each | each fileContents = bytes]
		ifNone:[].
	^cacheEntry