tests
testBehaviorRespectsPolymorphismWithTraitBehavior
	| repeatedMethodsThatDoNotAccessInstanceVariables differentMethodsWithSameSelector |
	"If the method is in Behavior and TraitBehavior it must access some instance variable, otherwise the method can be implemented in TBehavior"
	repeatedMethodsThatDoNotAccessInstanceVariables := self
		repeatedMethodsThatDoNotAccessInstanceVariablesBetween: Behavior
		and: TraitBehavior.
	self assert: repeatedMethodsThatDoNotAccessInstanceVariables size equals: 0.	"If the method is in Behavior and TraitBehavior, and they have different implementations, it must be declared in TBehavior as an explicitRequirement method"
	differentMethodsWithSameSelector := self differentMethodsWithSameSelectorBetween: Behavior and: TraitBehavior.
	differentMethodsWithSameSelector
		do: [ :selector | 
			(TBehavior >> selector) sourceCode.
			self assert: (TBehavior >> selector) isRequired ].	"Only a few methods are allowed to belong to one class and not to the other"	"If you want to remove methods for this list, then go ahead. But is NOT good idea add methods to this list"	"format -> the instance variable format belongs to Behavior but not to TraitBehavior, if we add the getter to TraitBehavior, what should we return?
	isBehavior -> returns true for all the classes but false for the traits, the implementation for traits (and the rest of the objects) is implemented in Object>>isBehavior
	layout -> is an instance variable of Behavior but not of TraitBehavior"
	self
		assert: (Behavior localSelectors difference: TraitBehavior localSelectors)
		equals: #(#format #layout #layout: #classLayout #classLayout: #isBehavior #metaLinkOptions 
			#handleFailingBasicNew #handleFailingBasicNew: #handleFailingFailingBasicNew
			#handleFailingFailingBasicNew: #byteSizeOfInstanceOfSize: #byteSizeOfInstance
			#ffiBindingOf: #ffiInstVarArgument:generator: #basicIdentityHash #elementSize #isCompiledMethodClass) asSet.	"localSelectors, localSelectors:, basicLocalSelectors, basicLocalSelectors: -> the instance variable localSelectors belongs to TraitBehavior but not to Behavior"	"browse is implemeted differently for traits, the implementation for classes is in Object"	"isTrait answers true for traits, for the rest of objects answer false, the implementation is in Object"
	self
		assert: (TraitBehavior localSelectors difference: Behavior localSelectors)
		equals: #(#isUsed #basicLocalSelectors #basicLocalSelectors: #isTrait #allInstances #allInstancesOrNil #allInstancesDo:) asSet