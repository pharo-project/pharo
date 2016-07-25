development support
updateForSpawnMethod: sourceSpec
	"This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"
	
	repositories := className := operator := loads := projectPackage := nil.
	sourceSpec ~~ nil ifTrue: [ versionString := sourceSpec versionString ].