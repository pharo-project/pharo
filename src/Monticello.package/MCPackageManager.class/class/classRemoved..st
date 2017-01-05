system changes
classRemoved: anEvent
	"Informs the registry who use to keep this class that its changed. Unlike #classModified:, class is not anymore in RPackages so it will not be found, that's why we look for system category instead if class is included or not"

	"Note. Class protocols returns symbols and not Protocol instances!"
	(RPackageOrganizer default
		packageMatchingExtensionName: anEvent categoryName) ifNotNil: [ :package |
			package mcWorkingCopy ifNotNil: [ :workingCopy | workingCopy modified: true ] ].
		
	anEvent classAffected protocols, anEvent classAffected theMetaClass protocols
		do: [ :aProtocolName | 
			(aProtocolName beginsWith: '*')
				ifTrue:
					[ (RPackageOrganizer default
						packageMatchingExtensionName: aProtocolName allButFirst trimBoth)
							ifNotNil: [ :package |
								package mcWorkingCopy ifNotNil: [ :workingCopy |
									workingCopy modified: true ] ] ] ]