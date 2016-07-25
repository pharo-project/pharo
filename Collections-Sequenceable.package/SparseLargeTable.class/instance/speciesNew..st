private
speciesNew: newSize

	^self species
		new: newSize 
		chunkSize: self chunkSize 
		arrayClass: self arrayClass
