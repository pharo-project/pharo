singleton
default
	"WARNING: Since this component can be changed (i.e. for testing) you should NOT use it directly. 
	  Use RPackage class>>#organizer instead"
	
	^ default ifNil: [  default := self new ]