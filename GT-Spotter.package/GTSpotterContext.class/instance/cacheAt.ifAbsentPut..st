public
cacheAt: aSymbol ifAbsentPut: aBlock
	^ self cache at: aSymbol ifAbsentPut: aBlock