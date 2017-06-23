*Iceberg-Core
filesDo: aBlockClosure 
	^ self filesDo: aBlockClosure binaryFilesDo: aBlockClosure hunksDo: nil linesDo: nil		
