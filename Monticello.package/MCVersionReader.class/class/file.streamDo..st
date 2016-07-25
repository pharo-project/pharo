reading
file: fileName streamDo: aBlock
	
	^ FileStream 
			readOnlyFileNamed: fileName 
			do: [:file | aBlock value: file]