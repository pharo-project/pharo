testing
readAllFrom: fd
	"MessageTally spyOn:[BMPReadWriter readAllFrom: FileDirectory default]"
	fd fileNames do:[:fName|
		(fName endsWith: '.bmp') ifTrue:[
			[Form fromBinaryStream: (fd readOnlyFileNamed: fName)] on: Error do:[:nix].
		].
	].
	fd directoryNames do:[:fdName|
		self readAllFrom: (fd / fdName)
	].