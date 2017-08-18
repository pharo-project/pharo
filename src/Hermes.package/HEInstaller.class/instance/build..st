creating classes
build: aHEClass
	| newMetaclass newClass superclass layoutType |
	newMetaclass := Metaclass new.
	
	superclass := self classNamed: aHEClass superclass.
	layoutType := self classNamed: aHEClass layoutClass.

	newMetaclass superclass: superclass class withLayoutType: FixedLayout slots: (self asSlots: aHEClass classInstancevariables).
	
	newClass := newMetaclass new.
	newClass setName: aHEClass className.
	
	newClass superclass: superclass withLayoutType: layoutType slots: (self asSlots: aHEClass instanceVariables).
	newClass environment: environment.	
	newClass basicCategory: aHEClass category.
	
	newClass
		declareClassVariables: (self asClassVariables:aHEClass classVariables);
		sharing: aHEClass sharedPools.	

	newClass classLayout slots do: [ :each | each installingIn: newClass ].
	newClass classLayout slots do: [ :each | each layoutChanged: newClass ].

	newClass traitComposition: (aHEClass traitComposition asLiteralIn: environment).
	newClass class traitComposition: (aHEClass classTraitComposition asLiteralIn: environment).
	
	newClass applyChangesOfNewTraitCompositionReplacing:#() asTraitComposition.
	newClass class applyChangesOfNewTraitCompositionReplacing:#() asTraitComposition.
	
	^newClass.
