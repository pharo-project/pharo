validation
validate
	"Issue 5: 
		[]no shadowing of names across project/package/group boundaries
	 Issue 6: 
		[]package version supplied for each package
		[]no released (stable/bleedingEdge) versions (only development version)
		package spec used instead of project spec in baseline (ConfigurationOfPharo-DaleHenrichs.50)
	Issue 20:
		[]pre and post load doits must be symbols, not blocks
	Others:
		[]symbolic versions can be resolved.
		[]Ensure that versions are Strings and symbolic versions are symbols.
	 	[]Ensure that imported versions are defined."

	self validatePragmas.
	^self validateProject