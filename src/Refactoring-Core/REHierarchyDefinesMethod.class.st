"
This condition checks in the class and its superclasses and subclasses if a class defines a given selector.
It could be a subclass of `REUpToRootDefinesMethod` since it extends the search.
"
Class {
	#name : 'REHierarchyDefinesMethod',
	#superclass : 'RBClassCondition',
	#instVars : [
		'selector'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}
