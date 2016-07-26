initializing
initializeUnicodeClass
	"self initializeUnicodeClass"
	Unicode 
		parseUnicodeDataFrom: self data readStream;
		parseCaseMappingFrom: self caseFolding readStream.