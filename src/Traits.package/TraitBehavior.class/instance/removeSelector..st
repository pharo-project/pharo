accessing method dictionary
removeSelector: aSelector
	"Assuming that the argument, selector (a Symbol), is a message selector 
	in my method dictionary, remove it and its method.
	
	If the method to remove will be replaced by a method from my trait composition,
	the current method does not have to be removed because we mark it as non-local.
	If it is not identical to the actual method from the trait it will be replaced automatically
	by #noteChangedSelectors:.
	
	This is useful to avoid bootstrapping problems when moving methods to a trait
	(e.g., from TBehavior to TMethodDictionaryBehavior). Manual moving (implementing
	the method in the trait and then remove it from the class) does not work if the methods
	themselves are used for this process (such as compiledMethodAt:, includesLocalSelector: or
	addTraitSelector:withMethod:)"
	
	| changeFromLocalToTraitMethod |
	changeFromLocalToTraitMethod := (self includesLocalSelector: aSelector)
		and: [self hasTraitComposition
				and: [self traitComposition includesMethod: aSelector]].

	changeFromLocalToTraitMethod
		ifFalse: [self basicRemoveSelector: aSelector]
		ifTrue: [self ensureLocalSelectors].
	self deregisterLocalSelector: aSelector.
	self noteChangedSelectors: (Array with: aSelector).
	
	self isTrait ifTrue: [ self notifyUsersOfChangedSelector: aSelector].