add-remove
atFont: aFreeTypeFont charCode: charCodeInteger type: typeFlag ifAbsentPut: aBlock 
	| charCodeTable typeTable entry v vSize |
	
	charCodeTable := fontTable at: aFreeTypeFont ifAbsentPut:[self dictionaryClass new: 60].
	typeTable := charCodeTable at: charCodeInteger ifAbsentPut:[self dictionaryClass new: 10].
	entry := typeTable at: typeFlag ifAbsent:[].
	entry 
		ifNotNil:[
			fifo moveDown: entry.
			^entry object].
	v := aBlock value.
	vSize := self sizeOf: v.
	(maximumSize notNil and:[vSize > maximumSize]) 
		ifTrue:[^v].  
	used := used + vSize.
	entry := (self fifoEntryClass new
		 font: aFreeTypeFont;
		charCode: charCodeInteger;
		type: typeFlag;
		object: v;
		yourself).
	typeTable at: typeFlag put: entry.
	fifo addLast: entry.
	maximumSize ifNotNil:[self shrinkTo: maximumSize].
	^v
	