preparation
exportSUnitProtocols
	"This method exports all protocols of the methots in the kernel and SUnit related packages in a single text file to be easily readed and imported from the 'in-midst of initialization' image."
		
	self exportProtocolsFor: self kernelPackageNames, self sUnitPackageNames to: self bootstrapCacheDirectory / 'protocolsSUnit.txt'.
