access
selectorsContaining: aString
	"Answer a list of selectors that contain aString within them. Case-insensitive.  Does return symbols that begin with a capital letter."

	| size selectorList ascii |

	selectorList := OrderedCollection new.
	(size := aString size) = 0 ifTrue: [^selectorList].

	aString size = 1 ifTrue:
		[
			ascii := aString first asciiValue.
			ascii < 128 ifTrue: [selectorList add: (OneCharacterSymbols at: ascii+1)]
		].

	(aString first isAlphaNumeric) ifFalse:
		[
			aString size = 2 ifTrue: 
				[Symbol hasInterned: aString ifTrue:
					[:s | selectorList add: s]].
			^selectorList
		].

	selectorList := selectorList copyFrom: 2 to: selectorList size.

	self allSymbolTablesDo: [:each |
		each size >= size ifTrue:
			[(each findSubstring: aString in: each startingAt: 1 
				matchTable: CaseInsensitiveOrder) > 0
						ifTrue: [selectorList add: each]]].

	^selectorList reject: [:each | "reject non-selectors, but keep ones that begin with an uppercase"
		each numArgs < 0 and: [each asString uncapitalized numArgs < 0]].

"Symbol selectorsContaining: 'scon'"