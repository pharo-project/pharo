classes and traits
classOrTraitNamed: aString
	"aString is either a class or trait name or a class or trait name followed by ' class' or 'classTrait' respectively.
	Answer the class or metaclass it names."

	| meta baseName |
	(aString endsWith: ' class')
		ifTrue: [ 
			meta := true.
			baseName := aString copyFrom: 1 to: aString size - 6 ]
		ifFalse: [ 
			(aString endsWith: ' classTrait')
				ifTrue: [ 
					meta := true.
					baseName := aString copyFrom: 1 to: aString size - 11 ]
				ifFalse: [ 
					meta := false.
					baseName := aString ] ].
	^self at: baseName asSymbol ifPresent:
            [ :global |
               (global isBehavior or: [global isTrait])ifTrue:
                       [ meta
                               ifFalse: [ global ]
                               ifTrue: [ global classSide ]]]