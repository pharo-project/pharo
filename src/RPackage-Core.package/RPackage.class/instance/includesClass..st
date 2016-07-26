testing
includesClass: aClass	
	"Returns true if the receiver includes aClass in the classes that are defined within it: only class definition are considered - not class extensions"
	
	^ self includesClassNamed: aClass theNonMetaClass name