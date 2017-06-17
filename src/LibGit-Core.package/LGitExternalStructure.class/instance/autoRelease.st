initialize-release
autoRelease
	"Some LGit structures need specialised free functions. Use the same autoRelease functionality
	as FFIExternalReference to achieve this.

	Note, subclasses should implement #resourceData
	and #finalizeResourceData: on class side"
	FFIExternalResourceManager addResource: self