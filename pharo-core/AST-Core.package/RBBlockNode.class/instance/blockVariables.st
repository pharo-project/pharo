accessing
blockVariables
	| vars |
	vars := super blockVariables asOrderedCollection.
	vars addAll: self argumentNames.
	^vars