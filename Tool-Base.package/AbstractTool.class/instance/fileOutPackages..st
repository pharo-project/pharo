package
fileOutPackages: aCollection

	self 
		do: [ :package |  package item package fileOut ]
		on: aCollection
		displaying: 'Filing out packages...'.