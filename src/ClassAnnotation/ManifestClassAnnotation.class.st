"
Deprecated. Class annotations described classes with objects answered by class-side methods marked with <classAnnotation>. Calypso used them to place its commands, tabs and table decorators, to select its contexts and to choose its scope and hierarchy providers. It now declares all of them with pragmas: <calypsoCommands> (see ClyCommandMap), <calypsoSelectionContext> (see ClySelectionContextDeclaration), <calypsoInheritedScopeProvider:> and <calypsoMethodVisibilityProvider:>. The guides doc/Calypso/Commands.md and doc/Calypso/CommandAnnotationsMigration.md of the Pharo repository explain them.
I am only used by the deprecated Commander 1 and by Commander-Calypso-Compatibility, which keeps the annotations of other projects working. I will be removed with them in Pharo 16.
"
Class {
	#name : 'ManifestClassAnnotation',
	#superclass : 'PackageManifest',
	#category : 'ClassAnnotation-Manifest',
	#package : 'ClassAnnotation',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestClassAnnotation class >> isDeprecated [
	"ClassAnnotation is only used by the deprecated Commander 1 and Commander-Calypso-Compatibility, and will be removed with them in Pharo 16."

	^ true
]
