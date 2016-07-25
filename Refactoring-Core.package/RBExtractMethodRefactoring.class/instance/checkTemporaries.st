transforming
checkTemporaries
	| temps accesses assigned |
	temps := self remainingTemporaries.
	accesses := temps select: [:each | extractedParseTree references: each].
	assigned := accesses select: [:each | extractedParseTree assigns: each].
	assigned isEmpty ifFalse: [self checkAssignments: assigned].
	^parameters := (accesses asOrderedCollection)
				removeAll: assigned;
				yourself