memory usage
instancesSizeInMemory
	"Answers the number of bytes consumed by all its instances including their object header"
	
	| bytes |
	bytes := 0.
	self allInstancesDo: [:each | bytes := bytes + each sizeInMemory  ].
	^ bytes