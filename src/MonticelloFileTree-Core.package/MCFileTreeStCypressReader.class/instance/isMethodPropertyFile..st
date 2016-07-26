private
isMethodPropertyFile: entry
	^ entry name = 'methodProperties.ston' or: [ entry name = 'methodProperties.json']