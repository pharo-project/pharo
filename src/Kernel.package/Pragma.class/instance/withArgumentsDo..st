processing
withArgumentsDo: aBlock
	"Pass the arguments of the receiving pragma into aBlock and answer the result."
	
	^ aBlock valueWithArguments: self arguments