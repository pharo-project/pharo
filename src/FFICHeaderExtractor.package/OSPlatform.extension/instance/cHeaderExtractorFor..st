*FFICHeaderExtractor
cHeaderExtractorFor: aSharedPool
	"IMPORATNT: first read comment of FFISharedPool #cHeaderExtractorFor:
	Subclass should likely check if platform is 32 or 64 bits and depending on that
	send the correct method (from 'platform' protocol) to FFISharedPool.
	
	Answers an instance of a FFICHeaderExtractor specially created for this platform.
	"
	^ self subclassResponsibility 