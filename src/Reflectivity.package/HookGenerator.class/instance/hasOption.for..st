options
hasOption: aSymbol for: aLink
	"take the class and method overrides to options into account"
	^(node methodNode optionsFor: aLink) includes: aSymbol.