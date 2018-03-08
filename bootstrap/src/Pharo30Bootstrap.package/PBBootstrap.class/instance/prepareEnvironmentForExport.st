preparation
prepareEnvironmentForExport
	| allPackages |

	allPackages := ringEnvironment packages collect: #name.

	(allPackages reject: [ :x | #BaselineOfPharoBootstrap asClass kernelPackageNames includes: x ])
		do: [ :x | 
			| p |
			p := ringEnvironment ask packageNamed: x.
			p extensionMethods do: [ :e | e methodClass removeLocalMethod: e ].
			p definedBehaviors
				do: [ :e | 
					ringEnvironment removeBehavior: e.
					ringEnvironment removeBehavior: e classSide
					].
			ringEnvironment removePackage: p].

	ringEnvironment cleanGlobalVariables.
	ringEnvironment addGlobalsNamed: #(#Smalltalk #SourceFiles #Transcript #Undeclared #Display #TextConstants  #Sensor #Processor #SystemOrganization).
	ringEnvironment clean
