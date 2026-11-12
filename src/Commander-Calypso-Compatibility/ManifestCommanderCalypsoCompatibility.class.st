"
Deprecated. I keep the class annotations of Calypso commands (ClyCommandActivationStrategy and its subclasses), tabs, table decorators, selection contexts and providers working while projects move to declarations with the <calypsoCommands> pragma (see ClyCommandMap). ClyCommandRegistry adds the annotations I hold to the commands it answers. ClyCommandDeclarationGenerator converts the annotations of some packages into declarations. I will be removed in Pharo 16. The migration is explained in doc/Calypso/CommandAnnotationsMigration.md of the Pharo repository.
"
Class {
	#name : 'ManifestCommanderCalypsoCompatibility',
	#superclass : 'PackageManifest',
	#category : 'Commander-Calypso-Compatibility-Manifest',
	#package : 'Commander-Calypso-Compatibility',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestCommanderCalypsoCompatibility class >> isDeprecated [
	"Commander v1 is deprecated and will be removed in Pharo 16."

	^ true
]
