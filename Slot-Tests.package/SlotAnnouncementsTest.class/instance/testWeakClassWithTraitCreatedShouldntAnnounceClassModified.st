tests-integration
testWeakClassWithTraitCreatedShouldntAnnounceClassModified

	self subscribeOn: ClassModifiedClassDefinition.	

	"Create a class with a trait composition"
	aClass := Object 
		weakSubclass: self aClassName
		uses: TOne 
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assert: self collectedAnnouncements size isZero.
	self assert: aClass classLayout isVariable.
	self assert: aClass classLayout isWeak.
	self assert: (aClass traitComposition syntacticallyEquals: TOne asTraitComposition).