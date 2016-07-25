tests-integration
testVariableByteClassWithTraitCreatedShouldntAnnounceClassModified

	self subscribeOn: ClassModifiedClassDefinition.	

	"Create a class with a trait composition"
	aClass := Object 
		variableByteSubclass: self aClassName
		uses: TOne 
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assert: self collectedAnnouncements size isZero.
	self assert: aClass classLayout isVariable.
	self assert: aClass classLayout isBits.
	self assert: aClass classLayout isBytes.
	self assert: (aClass traitComposition syntacticallyEquals: TOne asTraitComposition).