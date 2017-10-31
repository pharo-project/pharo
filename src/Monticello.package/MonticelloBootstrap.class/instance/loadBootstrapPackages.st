bootstrapping
loadBootstrapPackages

MCCacheRepository cacheDirectory: directory.

	(self bootstrapVersionsExcept: [ :package | 
		#('Shift-ClassBuilder' 'Monticello')
			includes: package name])
		do: [ :version | self loadPackageVersion: version ].

	"We load ShiftClassBuilder at the end because it has a patch to work in the bootstrap process. The patch is used when there is no source."

	self loadBootstrapPackageNamed: 'Shift-ClassBuilder'.

	"We load lastly monticello because we are using it to reload itself.
	Othewise, we should handle the case that after loading monticello we should return to a safe place from the stack where no old methods or classes are used."
	self loadBootstrapPackageNamed: 'Monticello'
