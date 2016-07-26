*zinc-resource-meta-core
asZnUrl
	"Convert the receiver in a file:// ZnUrl"

	| fileUrl |
	fileUrl := ZnUrl new.
	fileUrl scheme: #file.
	self directory pathSegments do: [ :each | fileUrl addPathSegment: each ].
	fileUrl addPathSegment: self localName.
	^ fileUrl