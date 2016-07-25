system integration
hasPackageExactlyMatchingExtensionName: anExtensionName
	| extension |
	extension := anExtensionName asSymbol.
	"fast version"
	(self includesPackageNamed: extension)
		ifTrue: [  ^ true ].
		
	"slow version ignoring case"
	packages keysDo: [ :aSymbol | 
		(aSymbol sameAs: extension)
			ifTrue: [ ^ true]].
	^ false