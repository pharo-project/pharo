preparation
exportKernelProtocols
	"This method exports all protocols of the methots in the kernel in a single text file to be easily readed and imported from the 'in-midst of initialization' image."
		
	self exportProtocolsFor: self kernelPackageNames to: self bootstrapCacheDirectory / 'protocolsKernel.txt'.
	
	