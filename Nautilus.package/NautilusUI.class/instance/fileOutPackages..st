package
fileOutPackages: aCollection
	self 
		do: [ :package | package asNautilusSelection item fileOut ]
		on: aCollection
		displaying: 'Filing out packages...'.