testing
displayAllFrom: fd
	"BMPReadWriter displayAllFrom: FileDirectory default"
	fd fileNames do:[:fName|
		(fName endsWith: '.bmp') ifTrue:[
			[(Form fromBinaryStream: (fd readOnlyFileNamed: fName)) display.
			Display forceDisplayUpdate] on: Error do:[:nix|].
		].
	].
	fd directoryNames do:[:fdName|
		self displayAllFrom: (fd / fdName)
	].