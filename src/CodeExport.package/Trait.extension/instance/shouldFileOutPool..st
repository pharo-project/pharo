*CodeExport
shouldFileOutPool: aPoolName
	"respond with true if the user wants to file out aPoolName"
	^self confirm: ('FileOut the sharedPool ', aPoolName, '?')