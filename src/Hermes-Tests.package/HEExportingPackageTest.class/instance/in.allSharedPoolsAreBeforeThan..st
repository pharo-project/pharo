as yet unclassified
in: aSeq allSharedPoolsAreBeforeThan: aClass
	| myIndex realClass |
	myIndex := aSeq indexOf: aClass.
	realClass := Smalltalk at: aClass className.
	^ realClass sharedPools allSatisfy: [ :sharedPool | 
		(aSeq detectIndex: [ :e | e className = sharedPool name ] ifNone: [ -1 ]) < myIndex ]