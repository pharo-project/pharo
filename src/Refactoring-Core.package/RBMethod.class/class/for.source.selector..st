instance creation
for: aRBClass source: aString selector: aSelector 
	^(self new)
		modelClass: aRBClass;
		selector: aSelector;
		source: aString;
		yourself