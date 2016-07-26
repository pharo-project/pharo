mappings
mappingToIdentifiers
	"Return a dictionay mapping from encoding specifications to a list of encoding names."
	
	^ Dictionary newFromPairs: #( 
		#iso88591Mapping #('iso88591' 'latin1')
		#asciiMapping #('ascii') )