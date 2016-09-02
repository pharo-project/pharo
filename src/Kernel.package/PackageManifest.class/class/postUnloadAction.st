unloading
postUnloadAction

	"override to return block that will be executed after the package unloading. Keep in mind that all package classes are already removed from the system including the manifest class"
	
	^ [ "do nothing" ]