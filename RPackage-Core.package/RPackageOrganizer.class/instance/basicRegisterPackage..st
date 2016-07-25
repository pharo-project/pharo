private registration
basicRegisterPackage: aPackage
	"A new package is now available and declared in the receiver. Note that it is a low level implementation method since it does not deal with package contained information and does not update the related mapping tables."
	
	packages 
		at: aPackage name asSymbol 
		put: aPackage.
	"register mc package"
	MCWorkingCopy forPackage: (MCPackage named: aPackage name).
	
	^ aPackage