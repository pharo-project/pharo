fileIn/Out
doesNotIncludeInstanceOrSuperclassesFor: aClass in: unprocessedClasses cache: cache 
	| soleInstance |
	soleInstance := aClass soleInstance.
	^ (unprocessedClasses includes: soleInstance) not and: [
				self hasNoSuperclassesOf: soleInstance in: unprocessedClasses cache: cache]