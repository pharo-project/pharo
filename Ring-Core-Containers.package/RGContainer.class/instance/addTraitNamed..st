adding/removing
addTraitNamed: traitName 
	"Creates a trait with the given name"
	self addClass:  (RGTraitDefinition named: traitName)