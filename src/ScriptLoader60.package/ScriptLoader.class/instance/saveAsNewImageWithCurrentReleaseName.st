private
saveAsNewImageWithCurrentReleaseName
	| file |
	file := FileSystem workingDirectory  / (('Pharo-', self currentUpdateVersionNumber asString), '-Release'), Smalltalk imageSuffix.
	Smalltalk image saveAs: (file nextVersion basename copyUpToLast: Path extensionDelimiter). 