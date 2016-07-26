private
itemsRequiring: anObject
	^ required at: anObject ifAbsentPut: [Set new]