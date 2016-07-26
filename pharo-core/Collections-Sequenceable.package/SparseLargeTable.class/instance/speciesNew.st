private
speciesNew

	^self species
		new: self size 
		chunkSize: self chunkSize 
		arrayClass: self arrayClass
