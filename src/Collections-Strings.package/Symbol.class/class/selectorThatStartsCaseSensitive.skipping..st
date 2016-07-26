accessing
selectorThatStartsCaseSensitive: leadingCharacters skipping: skipSym
	"Same as thatStartsCaseSensitive:skipping: but on the SelectorTable only"
	| size firstMatch key |

	size := leadingCharacters size.
	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].
	firstMatch := leadingCharacters at: 1.
	size > 1 ifTrue: [key := leadingCharacters copyFrom: 2 to: size].
	self selectorTable do: [:each |
			(each notNil and: [each size >= size]) ifTrue:
				[
					((each at: 1) == firstMatch and:
						[key == nil or:
							[(each findString: key startingAt: 2 caseSensitive: true) = 2]])
								ifTrue: [^each]
				]
		] after: skipSym.

	^nil
