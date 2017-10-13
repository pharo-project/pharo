preparation
exportPackageNamesFor: packageNames to: aTextFile

	"export list of packages for the RPackage initialization. Eech package on standalone line"
		
	aTextFile ensureDelete.
	aTextFile writeStreamDo: [:stream | 
		packageNames do: [ :each | stream nextPutAll: each; cr ]].