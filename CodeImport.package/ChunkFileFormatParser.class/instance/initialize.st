initialization
initialize
	super initialize.
	parsedDeclarations := OrderedCollection new.
	classSelector := self defaultClassSelector.
	classTraitSelector := self defaultClassTraitSelector.
	commentStampSelector := self defaultCommentStampSelector.
	methodsForSelector := self defaultMethodsForSelector.
	reorganizeSelector := self defaultReorganizeSelector