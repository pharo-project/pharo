"
Deprecated: commands, tabs and table decorators are declared on contexts with the <calypsoCommands> pragma (ClyCommandMap). I am kept, in the deprecated Commander-Calypso-Compatibility package, for the projects which still use me, and I will be removed in Pharo 16. doc/Calypso/CommandAnnotationsMigration.md of the Pharo repository explains the migration.

I am an annotation context which describes the full browser contexts where no class is selected.
"
Class {
	#name : 'ClyEmptyClassSelectionContext',
	#superclass : 'AnnotationContext',
	#category : 'Commander-Calypso-Compatibility-Annotations',
	#package : 'Commander-Calypso-Compatibility',
	#tag : 'Annotations'
}

{ #category : 'testing' }
ClyEmptyClassSelectionContext >> describes: aBrowserContext [
	^(aBrowserContext isKindOf: ClyFullBrowserContext)
		and: [aBrowserContext isClassSelected not]
]
