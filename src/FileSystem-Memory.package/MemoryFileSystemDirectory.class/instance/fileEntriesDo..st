enumeration
fileEntriesDo: aBlock
	
	entries keys sort do: [ :fileName|
		aBlock value: (entries at: fileName)].