accessing
handlesEncoding: string
	"Return true when my instances handle the encoding described by string.
	Note that in principle I could handle latin1 (iso-8859-1) and ASCII, 
	although that is not completely correct. 
	To get maximum efficiency, it remains an option."
	
	^ (self canonicalEncodingIdentifier: string) = 'null'