instance creation
forLogicalFont: aLogicalFont fileInfo: aFreeTypeFileInfoAbstract
	| pointSize index |
	pointSize := aLogicalFont pointSize.
	index := aFreeTypeFileInfoAbstract index.  
	^aFreeTypeFileInfoAbstract isEmbedded
		ifTrue:[
			self 
				fromBytes: aFreeTypeFileInfoAbstract fileContents 
				pointSize: pointSize 
				index: index]
		ifFalse:[
			self 
				fromFile: aFreeTypeFileInfoAbstract absolutePath
				pointSize: pointSize 
				index: index]