extracting
extractAndStoreHeadersInformation
	"Main API method used by FFISharedPool. It first extracts all the constants values (defined in C header files) and then creates a smalltalk init method which is then compiled/installed in the shared pool"
	self extractHeadersInformation.	
	self storeFromExtracted.
