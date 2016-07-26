loading and updating
validCachedInfoFor: aFile index: i
	"answer info from cache if the file on the disk has the same size/timestamp as the cached info, otherwise answer nil"
	| cacheEntry fileSize modificationTime path |

	fileSize := aFile size.
	modificationTime :=  aFile modificationTime.
	path := aFile fullName.
	cacheEntry := (fileInfoCache at: {fileSize. i} ifAbsentPut: [ Set new ])
		detect: [ :each | 
			each modificationTime = modificationTime
			and: [ each absolutePath = path ]]
		ifNone:[].
	"cacheEntry ifNotNil:[Transcript cr; show: 'from cache : ', cacheEntry asString]."
	^ cacheEntry