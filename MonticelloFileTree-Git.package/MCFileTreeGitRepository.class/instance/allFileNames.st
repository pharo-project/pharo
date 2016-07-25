accessing
allFileNames
	"Switch to caching for speed."

	cacheFileNames == true
		ifFalse: [ ^ self loadAllFileNames ].
	^ allFileNames ifNil: [ allFileNames := self loadAllFileNames ]