accessing
at: headerName add: value
	"Store value under headerName, optionally turning it into a multi-valued header 
	when a value was already present"
	
	^ self headers at: (self normalizeHeaderKey: headerName) add: value 
		
