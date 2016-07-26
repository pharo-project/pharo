actions
loadLatest
	"Warning: loading the latest version of a package could be tricky if there are other packages from the same repository already loaded in the image, with incompatible versions. We should check that situation and produce an alert."
	(repository latestVersionForPackage: package) load.
	
	repository announcer announce: IcePackageLoaded new.
	repository refresh
	