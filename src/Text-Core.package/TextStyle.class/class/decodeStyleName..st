utilities
decodeStyleName: styleName 
	"Given a string styleName, return a collection with: 
	 
	* [1] the probable Pharo emphasis code, which is a bit combination of: 
	1	bold 
	2	italic 
	4	underlined 
	8	narrow 
	16	strikeout 
	 
	* [2] the base style name without the modifiers (can be empty)
	* [3] the modifiers in the order they were found 
	* [4] the codes for those modifiers, in the same order
	"
	| decoder keys modifiers modifierCodes baseName styleCode matchedKey |

	decoder := self styleDecoder.

	modifiers := OrderedCollection new.
	modifierCodes := OrderedCollection new.
	keys := decoder keys asArray
				sort: [:a :b | a size > b size].
	styleCode := 0.
	baseName := styleName asString.
	[matchedKey := keys
				detect: [:k | baseName endsWith: k]
				ifNone: [].
	matchedKey notNil]
		whileTrue: [| last code | 
			last := baseName size - matchedKey size.
			last > 0
				ifTrue: [('- ' includes: (baseName at: last))
						ifTrue: [last := last - 1]].
			baseName := baseName copyFrom: 1 to: last.
			code := decoder at: matchedKey.
			styleCode := styleCode + code.
			modifiers addFirst: matchedKey.
			modifierCodes addFirst: code.
	].
	^ {styleCode. baseName. modifiers. modifierCodes }