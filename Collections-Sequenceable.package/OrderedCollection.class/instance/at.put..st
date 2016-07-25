accessing
at: anInteger put: anObject
	"Put anObject at element index anInteger. at:put: cannot be used to
	append, front or back, to an ordered collection; it is used by a
	knowledgeable client to replace an element."

	| index |
	anInteger < 1
		ifTrue: [ self errorSubscriptBounds: anInteger ].
	(index := anInteger + firstIndex - 1) > lastIndex
		ifTrue: [ self errorSubscriptBounds: anInteger ].
	^ array at: index put: anObject