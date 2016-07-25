private
initializeSelectorsFor: aClass 
	|excludedClasses|
	selectors reset.
	excludedClasses := (expanded ifTrue: [#()] ifFalse: [Object withAllSuperclasses]).
	selectors addAll: ((aClass allSelectorsWithout: excludedClasses)
						collect: [:each | NECSelectorEntry contents: each type: #selector])