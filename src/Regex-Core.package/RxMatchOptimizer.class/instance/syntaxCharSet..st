double dispatch
syntaxCharSet: charSetNode 
	"All these (or none of these) characters is the prefix."

	charSetNode isNegated
		ifTrue: [nonPrefixes addAll: (charSetNode enumerableSetIgnoringCase: ignoreCase)]
		ifFalse: [prefixes addAll: (charSetNode enumerableSetIgnoringCase: ignoreCase)].
	charSetNode hasPredicates ifTrue: 
			[charSetNode isNegated
				ifTrue: [nonPredicates addAll: charSetNode predicates]
				ifFalse: [predicates addAll: charSetNode predicates]]