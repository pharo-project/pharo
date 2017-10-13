*CodeExport
shouldFileOutPools
	"respond with true if the user wants to file out the shared pools"
	^self confirm: 'FileOut selected sharedPools?'