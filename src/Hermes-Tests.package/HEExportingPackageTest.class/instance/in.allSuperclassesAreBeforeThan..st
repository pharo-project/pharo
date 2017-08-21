as yet unclassified
in: aSeq allSuperclassesAreBeforeThan: aClass
	| myIndex realClass |
	myIndex := aSeq indexOf: aClass.
	realClass := Smalltalk at: aClass className.
	^ realClass allSuperclasses allSatisfy: [ :superclass | (aSeq detectIndex: [ :e | e className = superclass name ] ifNone: [ -1 ]) < myIndex ]