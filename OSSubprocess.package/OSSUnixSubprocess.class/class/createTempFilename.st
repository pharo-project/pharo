temp files
createTempFilename
	"Read comment of createTempFileToBeUsedAsReadStreamOn:.
	
	This answers just ONE WAY of defining a temp filename"
	
	^ self name , '-p', OSSVMProcess vmProcess pid asString, '-', UUID new printString, '.deleteme'
	