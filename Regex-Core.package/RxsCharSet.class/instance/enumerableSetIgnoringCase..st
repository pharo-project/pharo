privileged
enumerableSetIgnoringCase: aBoolean
	"Answer a collection of characters that make up the portion of me
	that can be enumerated."

	| set |
	set := Set new.
	elements do:
		[:each |
		each isEnumerable ifTrue:
			[each enumerateTo: set ignoringCase: aBoolean]].
	^set