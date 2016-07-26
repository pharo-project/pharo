add-remove
atFont: aFreeTypeFont charCode: charCodeInteger type: typeFlag put: anObject 
	| charCodeTable typeTable anObjectSize oldEntry oldEntrySize entry |
	
	anObjectSize := self sizeOf: anObject.
	(maximumSize notNil and:[anObjectSize > maximumSize]) 
		ifTrue:[^anObject].  
	(charCodeTable := fontTable at: aFreeTypeFont ifAbsentPut:[self dictionaryClass new: 60])
		ifNotNil:[
			(typeTable := charCodeTable at: charCodeInteger ifAbsentPut:[self dictionaryClass new: 10])
				ifNotNil:[
					oldEntry := typeTable at: typeFlag ifAbsent:[].
					oldEntrySize := (oldEntry 
						ifNil:[0] 
						ifNotNil:[self sizeOf: oldEntry object]). 
					entry := (self fifoEntryClass new
						font: aFreeTypeFont;
						charCode: charCodeInteger;
						type: typeFlag;
						object: anObject;
						yourself).
					typeTable at: typeFlag put: entry]].
	used := used + anObjectSize - oldEntrySize.
	oldEntry ifNotNil: [fifo remove: oldEntry].
	fifo addLast: entry.
	maximumSize ifNotNil:[self shrinkTo: maximumSize].
	^anObject
	