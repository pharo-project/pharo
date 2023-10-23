Extension { #name : 'SycMigrateReferencesOfClassCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycMigrateReferencesOfClassCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.3 for: ClySourceCodeContext
]
