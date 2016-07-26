development support
packagesNeedSaving: aVersionString
	"Answer a collection of associations (package -> repository) representing the packages 
	 reachable from this project that need to be saved"

	| packages |
	packages := Set new.
	(self version: aVersionString) spec 
		packagesNeedSavingVisited: (Set with: self configuration class name asString)
		into: packages.
	^packages