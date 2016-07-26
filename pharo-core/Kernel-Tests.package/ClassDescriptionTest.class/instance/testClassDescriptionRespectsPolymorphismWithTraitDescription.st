tests
testClassDescriptionRespectsPolymorphismWithTraitDescription
	| repeatedMethodsThatDoNotAccessInstanceVariables differentMethodsWithSameSelector |
	"If the method is in ClassDescription and TraitDescription it must access some instance variable, otherwise the method can be implemented in TClassDescription"
	repeatedMethodsThatDoNotAccessInstanceVariables := self
		repeatedMethodsThatDoNotAccessInstanceVariablesBetween: ClassDescription
		and: TraitDescription.
	self assert: repeatedMethodsThatDoNotAccessInstanceVariables size = 0.	
	
	"If the method is in ClassDescription and TraitDescription, and they have different implementations, it must be declared in TClassDescription as an explicitRequirement method"
	differentMethodsWithSameSelector := self
		differentMethodsWithSameSelectorBetween: ClassDescription
		and: TraitDescription.
	differentMethodsWithSameSelector
		do: [ :selector | 
			(TClassDescription >> selector) sourceCode.
			self assert: (TClassDescription >> selector) isRequired ].	
			
	"Only a few methods are allowed to belong to one class and not to the other
	If you want to remove methods for this list, then go ahead. But is NOT good idea add methods to this list
	#superclass:layout: -> it accesses instance variables that are in ClassDescription but not in TraitDescription
	#layout is the getter of the instance variable layout (which is in ClassDescription but not in TraitDescription)
	#baseClass and #classClass have their equivalens in TraitDescription (baseTrait classTrait )"
	self
		assertCollection: (ClassDescription localSelectors difference: TraitDescription localSelectors)
		equals:
			#(#superclass:layout: #baseClass #superclass:withLayoutType:slots: #classClass #addSlot:) asSet.	
	"#isClassTrait, #baseTrait, #isBaseTrait and #classTrait have their equivalents in ClassDescription but with different name,
		the problem comes from the name of the selector, they are not good names
	#copyTraitExpresion and #addExclusionOf: have no equivalent in classes"
	self
		assertCollection: (TraitDescription localSelectors difference: ClassDescription localSelectors)
		equals:
			#(#isClassTrait #addExclusionOf: #copyTraitExpression #baseTrait #-  #isBaseTrait #classTrait syntacticallyEquals: instVarNames) asSet