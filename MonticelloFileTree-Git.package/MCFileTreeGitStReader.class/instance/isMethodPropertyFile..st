private
isMethodPropertyFile: entry
	| entryName |
	entryName := entry splitFileName last.
	^ entryName = 'methodProperties.ston' or: [ entryName = 'methodProperties.json']