loading
reloadBootstrapPackages
	
	DangerousClassNotifier disable.
	
	"get the list of packages from the configuration"
	self repository directory files sorted
		do: [ :each | | mcVersion |
			mcVersion := each basenameWithoutExtension.
			((mcVersion beginsWith: 'Slot-TheIntegrator.697') or: [mcVersion isEmpty])
				ifFalse: [ self loadPackage: mcVersion from: self repository ] ].
			
	"We load the Slot package at the end because it contains several patches"
	self loadPackage: 'Slot-TheIntegrator.697' from: self repository.
	
	"check that rpackages are initialized"
	RPackageOrganizer default packageNames logCr.