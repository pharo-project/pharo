menu spec
revertPackageMenuSpec

	^ #(#('Revert package...' #revertPackage) 
			   #('Recompile package' #recompilePackage)
			   #('Backport package...' #backportChanges)
			   #('Inspect package' #inspectWorkingCopy))
