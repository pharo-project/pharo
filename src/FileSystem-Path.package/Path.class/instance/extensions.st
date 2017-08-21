accessing
extensions
	"Return the extensions of the receiver in order of appearance"
	"(Path from: '/foo/bar.tar.gz') extensions"
	^ (self extensionDelimiter split: self basename) allButFirst