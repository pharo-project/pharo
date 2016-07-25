A MCPackageLoader is responsible for loading packages.  It gets used by VersionLoader, so it is eventually responsible for loading everything.

Instance Variables
	additions:		<Definitions>  Definitions that need to be added
	errorDefinitions:		<Object>
	obsoletions:		<Object>
	provisions:		<Object>
	removals:		<Object>
	requirements:		<Object>
	unloadableDefinitions:		<Object>
	methodAdditions  <MethodAdditions> MethodDefinitions corresponding to the Definitions in "additions" that have been added so far.
