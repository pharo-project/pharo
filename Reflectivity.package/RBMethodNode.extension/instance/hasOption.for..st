*Reflectivity
hasOption: aSymbol for: aLink
	"take the class and method overrides to options into account"
	^(self optionsFor: aLink) includes: aSymbol.