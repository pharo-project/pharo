tests-integration
testVariableByteClassWhenTraitCompositionChangedShouldAnnounceClassModified

	self subscribeOn: ClassModifiedClassDefinition.	

	"Create a class without trait composition"
	Object 
		variableByteSubclass: self aClassName
		uses: { } 
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assert: self collectedAnnouncements size isZero.
	
	"Set a new trait composition"
	Object 
		variableByteSubclass: self aClassName
		uses: TOne 
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.
	
	self assert: self collectedAnnouncements size equals: 1.
	announcement := self collectedAnnouncements first.
	self assert: (announcement oldClassDefinition traitComposition syntacticallyEquals: {} asTraitComposition).	
	self assert: (announcement newClassDefinition traitComposition syntacticallyEquals: TOne asTraitComposition).