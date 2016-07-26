private
similarInstance: newSize

	^self class
		new: newSize 
		chunkSize: self chunkSize 
		arrayClass: self arrayClass
