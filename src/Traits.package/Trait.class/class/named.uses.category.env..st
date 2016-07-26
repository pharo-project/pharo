instance creation
named: aSymbol uses: aTraitCompositionOrCollection category: aString env: anEnvironment
	| trait oldTrait systemCategory oldCategory |
	
	systemCategory := aString asSymbol.
	trait := anEnvironment
		at: aSymbol
		ifAbsent: [nil].
	oldTrait := trait copy.
	"Since there some strange side effect who changes teh category before being able to 
	  actually update (and that causes a problem on RPackage organization), we take also the 
	  category"
	oldCategory := oldTrait ifNotNil: [ oldTrait category ].
	trait := trait ifNil: [ super new ].
	
	(trait isKindOf: Trait) ifFalse: [
		^self error: trait name , ' is not a Trait'].
	trait
		setName: aSymbol
		andRegisterInCategory: systemCategory
		environment: anEnvironment.
		
	trait setTraitComposition: aTraitCompositionOrCollection asTraitComposition.
	
	"... notify interested clients ..."
	oldTrait ifNil: [
		SystemAnnouncer uniqueInstance 
			classAdded: trait 
			inCategory: systemCategory.
		^trait].

	SystemAnnouncer uniqueInstance traitDefinitionChangedFrom: oldTrait to: trait.

	systemCategory ~= oldCategory
		ifTrue: [ 
			SystemAnnouncer uniqueInstance 
				class: trait 
				recategorizedFrom: (oldCategory ifNil: [ #Unclassified ])
				to: systemCategory].
		
	^ trait