tests-integration
testFixedClassWithTraitCreatedShouldntAnnounceClassModified

	self subscribeOn: ClassModifiedClassDefinition.	

	"Create a class with a trait composition"
	aClass := Object 
		subclass: self aClassName
		uses: TOne 
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assert: self collectedAnnouncements size isZero.
	self deny: aClass classLayout isVariable.
	self assert: (aClass traitComposition syntacticallyEquals: TOne asTraitComposition).