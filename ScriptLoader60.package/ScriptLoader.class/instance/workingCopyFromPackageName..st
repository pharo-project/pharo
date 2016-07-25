fixing stream
workingCopyFromPackageName: aString
	"self new workingCopyFromPackageName: '39Deprecated' "
	
	|pa|
	pa := MCPackage named: aString.
	^ pa workingCopy 
	