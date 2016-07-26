*zinc-resource-meta-filesystem
asZnUrl
	"Convert the receiver in a file:// ZnUrl.
	Only an absolute path can be represented as a file:// URL"
	
	^ self asAbsolute path asZnUrl