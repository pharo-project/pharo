private registration
basicUnregisterPackage: aPackage
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"
	
	packages
		removeKey: aPackage name asSymbol
		ifAbsent: [ self reportExtraRemovalOf: aPackage ].

	"unregister also mc package"
	self flag: #hack. "for decoupling MC"
	self class environment at: #MCWorkingCopy ifPresent: [ 
		aPackage mcPackage 
			ifNotNil: [ :mcPackage | mcPackage workingCopy unregister ]].