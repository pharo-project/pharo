own services
serviceGet
	"Answer a service for getting the entire file"

	^  (SimpleServiceEntry 
			provider: self 
			label: 'Get entire file' 
			selector: #get
			description: 'If the file has only been partially read in, because it is very large, read the entire file in at this time.')