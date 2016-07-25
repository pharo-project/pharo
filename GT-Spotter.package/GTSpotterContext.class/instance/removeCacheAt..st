public
removeCacheAt: aSymbol
	^ self cache removeKey: aSymbol ifAbsent: [ nil ]