add-remove
atFont: aFreeTypeFont charCode: charCodeInteger type: typeFlag 
	| entry charCodeTable typeTable |
	(charCodeTable := fontTable at: aFreeTypeFont ifAbsent:[])
		ifNotNil:[
			(typeTable := charCodeTable at: charCodeInteger ifAbsent:[])
				ifNotNil:[
					(entry := typeTable at: typeFlag ifAbsent:[])
						ifNotNil:[
							fifo moveDown: entry.
							^entry object]]].
	self error: 'Not found'