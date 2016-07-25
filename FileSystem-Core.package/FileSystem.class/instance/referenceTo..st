public
referenceTo: aResolvable 
	"Answer a reference to the argument from the context of the receiver filesystem.  	
		Example: Filesystem disk referenceTo: 'plonk.taz'"

	^ FileReference 
		fileSystem: self
		path: (self pathFromObject: aResolvable)