testing
shouldInheritSelectors
	"I should inherit from an Abstract superclass but not from a concrete one by default, unless I have no testSelectors in which case I must be expecting to inherit them from my superclass.  If a test case with selectors wants to inherit selectors from a concrete superclass, override this to true in that subclass."
	
	^self ~~ self lookupHierarchyRoot
		and: [self superclass isAbstract or: [self testSelectors isEmpty]]