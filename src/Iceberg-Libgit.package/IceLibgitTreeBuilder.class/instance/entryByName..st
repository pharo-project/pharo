querying
entryByName: aString
	^ self children 
		at: aString 
		ifAbsentPut: [  
			self class new 
				initializeWithParent: self 
				entryName: aString ]