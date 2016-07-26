accessing
removeMethod: aSymbol

	(self protocolsOfSelector: aSymbol) do: [ :p | p removeMethod: aSymbol ]