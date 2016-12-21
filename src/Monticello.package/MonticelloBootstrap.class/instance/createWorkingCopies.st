bootstrapping
createWorkingCopies
	
	"For each of the packages inside the image, excepted by the Unpackaged package, create a monticello working copy.
	This is achieved by creating an instance of a working copy and snapshotting it. This generates and registers the working copy automatically."
	(RPackageOrganizer default packageNames) asSortedCollection do: [:packageName |
		packageName = RPackage defaultPackageName ifFalse: [
			('Creating Snapshot of: ', packageName) logCr.
			(MCWorkingCopy forPackage: (MCPackage named: packageName)) snapshot ] ]