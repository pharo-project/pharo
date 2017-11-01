"
I create a Nautilus group with all the loaded packages from a configuration inside of it.
Use it by providing the way to get all the necessary informations to access data. See class side.

You can use it through the CatalogBrowser, Versionner or by hand with :

(ConfigurationOfXXX project version: aVersion) loadAndCreateGroupInNautilus 
"
Class {
	#name : #LoadedPackagesGroupCreatingCodeProvider,
	#superclass : #Object,
	#category : 'NautilusGroupAutoBuilder'
}

{ #category : #'public access' }
LoadedPackagesGroupCreatingCodeProvider class >> configurationName: configurationName versionString: versionString projectName: projectName [
	| record packagesToAddInGroup packageNames |
	packagesToAddInGroup := OrderedCollection new.
	packageNames := OrderedCollection new.
	record := ((Smalltalk globals at: configurationName asSymbol) project
		version: versionString)
		ignoreImage: true;
		record.
	"We get the name of the packages which are in the project"
	record loadDirective
		packageDirectivesDo: [ :directive | packageNames add: directive spec name ].
	"We get the packages which matche the packages name of the project, and we filter to not keep the configuration packages to build the list of packages to add in the group"
	packageNames
		do:
			[ :eachName | 
			((eachName beginsWith: 'ConfigurationOf') or: [ eachName beginsWith: 'BaselineOf' ])
				ifFalse:
					[ 
					packagesToAddInGroup 
						add:
							(RPackageOrganizer default packages detect: [ :package | package name = eachName ]) ] ].
	(Smalltalk tools browser groupsManager addAnEmptyDynamicGroupNamed: projectName)
		addPackages: packagesToAddInGroup.
	GroupAnnouncer uniqueInstance announce: (AGroupHasBeenAdded group: nil into: nil)
]
