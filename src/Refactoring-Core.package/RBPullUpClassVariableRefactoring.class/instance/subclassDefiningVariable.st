private-accessing
subclassDefiningVariable
	| subclasses |
	subclasses := class allSubclasses 
		select: [ :each | each isMeta not and: [ each directlyDefinesClassVariable: variableName ] ].
	subclasses isEmpty 
		ifTrue: [ self refactoringFailure: 'Could not find a class defining ' , variableName ].
	subclasses size > 1 
		ifTrue: [ self refactoringError: 'Multiple subclasses define ' , variableName ].
	^ subclasses asArray first