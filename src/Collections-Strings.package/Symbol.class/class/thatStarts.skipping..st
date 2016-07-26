access
thatStarts: leadingCharacters skipping: skipSym
	"Answer a selector symbol that starts with leadingCharacters.
	Symbols beginning with a lower-case letter handled directly here.
	Ignore case after first char.
	If skipSym is not nil, it is a previous answer; start searching after it.
	If no symbols are found, answer nil.
	Used by Alt-q (Command-q) routines"

	| size firstMatch key |

	size := leadingCharacters size.
	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].

	firstMatch := leadingCharacters at: 1.
	size > 1 ifTrue: [key := leadingCharacters copyFrom: 2 to: size].

	self allSymbolTablesDo: [:each |
			each size >= size ifTrue:
				[
					((each at: 1) == firstMatch and:
						[key == nil or:
							[(each findString: key startingAt: 2 caseSensitive: false) = 2]])
								ifTrue: [^each]
				]
		] after: skipSym.

	^nil

"Symbol thatStarts: 'sf' skipping: nil"
"Symbol thatStarts: 'sf' skipping: #sfpGetFile:with:with:with:with:with:with:with:with:"
"Symbol thatStarts: 'candidate' skipping: nil"
