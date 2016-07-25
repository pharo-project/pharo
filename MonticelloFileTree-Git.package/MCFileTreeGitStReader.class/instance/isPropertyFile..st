private
isPropertyFile: entry
	| entryName |
	entryName := entry splitFileName last.
	^ entryName = 'properties.ston' or: [ entryName = 'properties.json']