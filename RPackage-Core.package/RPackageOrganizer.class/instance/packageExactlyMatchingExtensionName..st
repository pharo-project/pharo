system integration
packageExactlyMatchingExtensionName: anExtensionName
	"only look for a package for which the name match 'anExtensionName', making no difference about case. Return nil if no package is found"
	
	| extension |
	extension := anExtensionName asSymbol.
	
	packages keysDo: [:aSymbol |
		(aSymbol sameAs: extension)
			ifTrue: [ ^ self packageNamed: aSymbol ]].

	^ nil