tests-integration
testVariableWordClassWhenTraitCompositionChangedShouldAnnounceClassModified

	self subscribeOn: ClassModifiedClassDefinition.	

	"Create a class without trait composition"
	Object 
		variableWordSubclass: self aClassName
		uses: { } 
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		package: self aCategory.

	self assert: self collectedAnnouncements size isZero.
	
	"Set a new trait composition"
	Object 
		variableWordSubclass: self aClassName
		uses: TOne 
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		package: self aCategory.
	
	self assert: self collectedAnnouncements size equals: 1.
	announcement := self collectedAnnouncements first.
	self assert: (announcement oldClassDefinition traitComposition syntacticallyEquals: {} asTraitComposition).	
	self assert: (announcement newClassDefinition traitComposition syntacticallyEquals: TOne asTraitComposition).