Extension { #name : #String }

{ #category : #'*NautilusCommon' }
String >> normalizedSelectorForComparison [
	"Returns a normalized version of the receiver without colons and in all lowercase.
	Optimized for Nautlius"

	| selectorString selectorStringSize normalizedSelectorString normalizedSelectorStringSize |
	"asString to convert Symbols"
	selectorString := self asString.
	selectorStringSize := self size.
	normalizedSelectorString := selectorString class new: selectorStringSize.
	normalizedSelectorStringSize := 0.
	"this inlined to:do: loop with direct string manipulation is faster than
	using copyWithout:, which relies on reject:"
	1 to: selectorStringSize do: [ :i | 
		| nextChar |
		(nextChar := selectorString at: i) == $:
			ifFalse: [
				normalizedSelectorString
					at: (normalizedSelectorStringSize := normalizedSelectorStringSize + 1)
					put: nextChar ] ].
	"faster to use asLowercase on the entire string after instead of on chars
	in the to:do: loop because of the bad Character>>#asLowercase implementation"
	^ (normalizedSelectorStringSize < selectorStringSize
		ifTrue: [ normalizedSelectorString first: normalizedSelectorStringSize ]
		ifFalse: [ selectorString ]) asLowercase
]
