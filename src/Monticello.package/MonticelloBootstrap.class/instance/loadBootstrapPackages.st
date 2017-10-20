bootstrapping
loadBootstrapPackages

	MCCacheRepository cacheDirectory: directory.

	(self bootstrapVersionsExcept: [ :package | 
		#('Collections-Sequenceable' 'Slot' 'Kernel' 'Monticello')
			includes: package name])
		do: [ :version | self loadPackageVersion: version ].

	self loadBootstrapPackageNamed: 'Kernel'.
	self loadBootstrapPackageNamed: 'Slot'.
	self loadBootstrapPackageNamed: 'Collections-Sequenceable'.
		
	"We load lastly monticello because we are using it to reload itself.
	Othewise, we should handle the case that after loading monticello we should return to a safe place from the stack where no old methods or classes are used."
	self loadBootstrapPackageNamed: 'Monticello'