private registration
basicRegisterPackage: aPackage
	"A new package is now available and declared in the receiver. Note that it is a low level implementation method since it does not deal with package contained information and does not update the related mapping tables."
	
	packages 
		at: aPackage name asSymbol 
		put: aPackage.

	"register mc package"
	self flag: #hack. "for decoupling MC"
	self class environment at: #MCWorkingCopy ifPresent: [ :wc |
		wc forPackage: ((self class environment at: #MCPackage) named: aPackage name)].
	
	^ aPackage