instance creation
parent
	"Answer a path that resolves to the parent of the current
	working directory. This is similar to .. in unix, but doesn't
	rely on actual hardlinks being present in the filesystem."

	^ RelativePath with: '..'