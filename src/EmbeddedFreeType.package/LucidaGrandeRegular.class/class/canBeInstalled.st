testing
canBeInstalled 
	"This font can be installed just in Macs *and* if font exists"
	^ OSPlatform current isMacOS and: [ self fontReference exists ]