preconditions
superclassEquivalentlyDefines: aSelector 
	| superTree myTree |
	class superclass isNil ifTrue: [^false].
	superTree := class superclass parseTreeFor: aSelector.
	myTree := class parseTreeFor: aSelector.
	(superTree isNil or: [myTree isNil]) ifTrue: [^false].
	^superTree equalTo: myTree exceptForVariables: #()