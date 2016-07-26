comparing
hash 
	"Ports which are equal should return hash. 
 
	Since equality is used only for matching port proxies (ParentPort) with 
	their actual port, the hash function is overridden only in ParentPort." 
	 
	^super hash