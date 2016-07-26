testing
includes: anAttribute
	"Note, we're testing an attribute value here.. the attribute set may already have 
	an attribute of same class, but it is important that it should match the attribute value as well,
	else receiver is not considered as one, which includes given attribute"
	^ ( attributes at: anAttribute class ifAbsent: [^false] ) = anAttribute