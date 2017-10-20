as yet unclassified
in: aSeq allSuperclassesAreBeforeThan: aClass
	| myIndex superclass superclassIndex |
	myIndex := aSeq indexOf: aClass.
	superclass := aSeq detect: [ :e | e className = aClass superclass ] ifNone: [ ^ true ].

	superclassIndex := aSeq indexOf: superclass.
	^ (superclassIndex < myIndex) and:[ self in:aSeq allSuperclassesAreBeforeThan: superclass].