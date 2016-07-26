scripts
modifySymbolicVersion: symbolicVersionSymbol for: configurationBasename attribute: attribute to: versionString
	"Set the value of the symbolic version for the given attribute to the given version string"

	<apiDocumentation>
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		modifySymbolicVersionMethodFor: symbolicVersionSymbol;
		addSymbolicSection: attribute version: versionString;
		commitMethod.
	^ toolbox