*metacello-mc
updateForSpawnMethod: sourceSpec
	"This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"

	| nm |
	nm := name.
	self projectReference updateForSpawnMethod: sourceSpec.
	super updateForSpawnMethod: sourceSpec.
	name := nm.