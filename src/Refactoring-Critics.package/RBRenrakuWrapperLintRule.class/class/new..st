instance creation
new: aRule

	| annotatedClass |
	
	annotatedClass := self newAnonymousSubclass.
	annotatedClass class compile:
		'uniqueIdentifierName ^ ',
		aRule class uniqueIdentifierName surroundedBySingleQuotes.

	^ annotatedClass basicNew
		initialize: aRule;
		yourself