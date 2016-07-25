tests-integration
testVariableWordClassWithTraitCreatedShouldntAnnounceClassModified

	self subscribeOn: ClassModifiedClassDefinition.	

	"Create a class with a trait composition"
	aClass := Object 
		variableWordSubclass: self aClassName
		uses: TOne 
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		package: self aCategory.

	self assert: self collectedAnnouncements size isZero.
	self assert: aClass classLayout isVariable.
	self assert: aClass classLayout isBits.
	self assert: aClass classLayout isWords.
	self assert: (aClass traitComposition syntacticallyEquals: TOne asTraitComposition).