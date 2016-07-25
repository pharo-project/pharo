convenience
downloadEntityTo: path
	"If I hold an entity, download it directly to a file.
	If path describes a file, use that. 
	If path describes a directory, use the last path segment of the URI to construct a file in that directory.
	The URI of the last request is used, which could make a difference when following redirects."

	| target |
	self entity ifNil: [ ^ self ].
	target := ZnFileSystemUtils downloadPathTo: path for: self request uri lastPathSegment.
	ZnFileSystemUtils
		newFileNamed: target
		do: [ :fileStream | 
			fileStream binary.
			self withProgressDo: [ self entity writeOn: fileStream ] ]