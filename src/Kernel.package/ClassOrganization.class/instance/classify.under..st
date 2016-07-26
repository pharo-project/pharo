backward compatibility
classify: aSymbol under: aProtocolName

	^ self 
		classify: aSymbol
		under: aProtocolName
		suppressIfDefault: true