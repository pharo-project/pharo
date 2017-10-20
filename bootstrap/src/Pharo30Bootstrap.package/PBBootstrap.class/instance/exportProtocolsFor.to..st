preparation
exportProtocolsFor: packageNames to: protocolsFile

	"This methods takes the packages from the (filetree) origin repository, reads all methods in these packages and export the their protocols in a single text file with the following format:
		[className]\tab[class is meta?]\tab[selector]\tab[protocol]."
		
	| originRepository |
	originRepository := self originRepository.
	protocolsFile ensureDelete.
	protocolsFile writeStreamDo: [:stream | | versions |
		versions := originRepository directory directories sorted.
		versions
			select: [ :file | packageNames includes: file basenameWithoutExtension ]
			thenDo: [:file | | version |
		    version := originRepository versionWithInfo: (originRepository versionInfoFromVersionNamed: file basenameWithoutExtension).
		    (version snapshot definitions select: #isMethodDefinition) do: [:def |
		      stream nextPutAll: def className asString; tab.
		      stream nextPutAll: def classIsMeta asString; tab.
		      stream nextPutAll: def selector asString; tab.
		      stream nextPutAll: def category asString; cr ]]]