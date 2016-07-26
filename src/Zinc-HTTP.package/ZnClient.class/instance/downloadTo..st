convenience
downloadTo: path
	"Setup streaming, do a GET request and download the entity directly to a file.
	If path describes a file, use that. 
	If path describes a directory, use the last path segment of the URI to construct a file in that directory.
	The URI of the first request is used, which could make a difference when following redirects.
	Return true when successful, false otherwise."

	| target |
	target := ZnFileSystemUtils downloadPathTo: path for: self request uri lastPathSegment.
	self
		streaming: true;
		get.
	^ self isSuccess
		ifTrue: [ 
			self downloadEntityTo: target.
			true ]
		ifFalse: [ false ]