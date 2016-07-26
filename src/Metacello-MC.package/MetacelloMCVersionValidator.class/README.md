Performs configuration validation.

For programmatically decoding reason codes use:

  MetacellMCVersionValidator fullDescriptionForReasonCode: <reasonCode>

Warning reason codes:

	#notDevelopmentVersion			- the symbolic version #development refers to a non-development literal version.
	#loadWarning 						- Warning signalled during load [load validation].
	#onlyBaselineVersion 				- one or more baseline versions have been defined, but no non-baseline versions are defined.
	#stableDevelopmentVersion		- a version whose blessing is #development has been declared as a #stable version

Critical Warning reason codes:

	#duplicateVersionDefinitions 		         - there are multiple pragma methods specifying the same version
	#loadDeprecation					         - deprecation warning signalled while loading configuration [load validation]
	#missingRecommendedProjectSpecField - missing recommended fields in project reference (versionString). The versionString should be specified so that #bleedingEdge loads will be predictable and repeatable
	#noLoadableVersions 				         - no non #baseline versions defined in configuration
	#noTests 							         - no test cases defined in loaded configuration [load validation]
	#noVersionSpecified 				         - no version defined for the project reference or package. The version specified in the baseline or the latest version of the project or package in the repository will be used.
	#packageNameMismatch 			         - the name in the packageSpec does not match the name of the mcz file
	#projectClassNameFileMismatch 	         - the class name of the configuration does not match the mcz file containing the configuration
	#testDeprecation 					         - deprecation warning signalled while running configuration tests [load validation]

Error reason codes:

	#cannotResolveVersion 			- the version (project reference or symbolic version) was not found in the specified configuration
	#duplicateNames 					- multiple independent definitions for an entity with same name (project, package, or group)
	#incompleteProjectSpec 			- missing required fields in project reference (className and/or repository)
	#incorrectVersionString 			- the version declared in pragma doesn't match version in versionSpec
	#invalidDoItSelector 				- doit select must be a Symbol
	#invalidVersionString 				- versionString must be a String
	#loadError 							- error occured while loading configuration [load validation]
	#missingVersionImport 			- version specified in import pragma not defined in configuration
	#noVersionsDefined 				- no usable baseline or version defined in configuration ... configuration cannot be loaded
	#projectCreationError 				- error occured while resolving project reference
	#shadowedNames 					- name duplication between packages and projects
	#testFailures						- test failures while running tests [load validation]
	#versionCompositionError 			- error while creating versionSpec from pragmas


