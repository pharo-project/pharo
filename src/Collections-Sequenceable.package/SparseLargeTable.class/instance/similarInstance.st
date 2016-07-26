private
similarInstance

	^self class
		new: self size 
		chunkSize: self chunkSize 
		arrayClass: self arrayClass
