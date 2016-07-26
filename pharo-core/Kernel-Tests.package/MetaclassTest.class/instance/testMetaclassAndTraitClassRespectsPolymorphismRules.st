tests
testMetaclassAndTraitClassRespectsPolymorphismRules
	| repeatedMethodsThatDoNotAccessInstanceVariables differentMethodsWithSameSelector |
	"If the method is in Metaclass and ClassTrait it must access some instance variable, otherwise the method can be implemented in TApplyingOnClassSide"
	repeatedMethodsThatDoNotAccessInstanceVariables := self
		repeatedMethodsThatDoNotAccessInstanceVariablesBetween: Metaclass
		and: ClassTrait.
	self assert: repeatedMethodsThatDoNotAccessInstanceVariables size = 0.	"If the method is in Metaclass and ClassTrait, and they have different implementations, it must be declared in TApplyingOnClassSide as an explicitRequirement method"
	differentMethodsWithSameSelector := self differentMethodsWithSameSelectorBetween: Metaclass and: ClassTrait.
	differentMethodsWithSameSelector
		do: [ :selector | 
			(TApplyingOnClassSide >> selector) sourceCode.
			self assert: (TApplyingOnClassSide >> selector) isRequired ].	"Only a few methods are allowed to belong to one class and not to the other
	It would be excelent to remove these methods somehow, but is NOT good idea add methods to this list"	"basicLocalSelectors basicLocalSelectors: localSelectors localSelectors: traitComposition traitComposition: -> accessors to instance variables that do not belong to ClassTrait
	baseClass classClass -> they have their equivalent for ClassTraits (baseTrait classTrait)
	environment postCopy -> the implementation for ClassTrait is in TBehavior
	veryDeepCopyWith: -> the implementation for ClassTraits is in Object
	fileOutOn: fileOutOn:initializing: -> the implementation for ClassTraits is in TClassDescription"
	self
		assert:
			(Metaclass localSelectors difference: ClassTrait localSelectors)
				equals:
					{#baseClass.
					#basicLocalSelectors:.
					#basicLocalSelectors.
					#classClass.
					#isClass.
					#environment.
					#fileOutOn:.
					#fileOutOn:initializing:.
					#postCopy.
					#traitComposition.
					#traitComposition:.
					#veryDeepCopyWith:.
					#addSlot:} asSet.	"initializeWithBaseTrait: asMCDefinition -> has no equivalent in classes
	baseTrait isClassTrait classTrait baseTrait isBaseTrait classTrait: -> they have their equivalent for Metaclasses
	compile:classified:withStamp:notifying:logSource: -> the implementation for Metaclasses is in TClassDescription
	copy -> the implementation for Metaclasses is in Object"
	self
		assert:
			(ClassTrait localSelectors difference: Metaclass localSelectors)
				=
					{#initializeWithBaseTrait:.
					#asMCDefinition.
					#baseTrait:.
					#isClassTrait.
					#classTrait.
					#baseTrait.
					#compile:classified:withStamp:notifying:logSource:.
					#isBaseTrait.
					#copy.
					#classTrait:} asSet