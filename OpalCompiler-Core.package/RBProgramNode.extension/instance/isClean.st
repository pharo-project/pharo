*opalcompiler-core
isClean
	^ self children allSatisfy: [ :child | child isClean ]