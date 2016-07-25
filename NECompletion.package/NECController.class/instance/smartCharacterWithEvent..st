keyboard
smartCharacterWithEvent: anEvent
	"char is extracted from anEvent, anEvent is passed because we may need it.
	We may remove char if this is not costly."

	| opposite previous next char insertion insertionCenter |
	char := anEvent keyCharacter.
	editor hasSelection
		ifTrue: [ 
			"we selected a piece of text and we type the same character that previously, so
		we unwrap it"
			"we selected a piece of text if the character is not a special one we do nothing."
			opposite := self smartCharactersMapping at: char ifAbsent: [ ^ nil ].	"else we wrap the selection"	"editor replaceSelectionWith: (String with: char) , (editor selection) , (String with: opposite)."	"ugly logic it should be encapsulated in the editor "
			editor enclose: anEvent.
			self invalidateEditorMorph.
			^ true ].	"we are not in a selection"
	opposite := self smartCharactersMapping
		at: char
		ifAbsent: [ 
			"if the character is not a special character"
			self smartInverseMapping at: char ifAbsent: [ ^ nil ].	"if the character is not a closing special character do nothing"	"The character is special"
			editor blinkPrevParen: char.
			(editor nextCharacterIfAbsent: [ ^ nil ]) = char
				ifFalse: [ ^ nil ].	"do not get this test but if we comment it out we cannot type closing ) anymore"
			editor selectFrom: editor startIndex + 1 to: editor startIndex.
			self invalidateEditorMorph.
			^ true ].
	previous := editor previousCharacterIfAbsent: [ Character space ].
	next := editor nextCharacterIfAbsent: [ Character space ].
	insertion := next isSeparator
				ifFalse: [ char asString ]
				ifTrue: [ 
					previous isSeparator
						ifFalse: [ char asString ]
						ifTrue: [ self newSmartCharacterInsertionStringForLeft: char right: opposite ]].
	editor replaceSelectionWith: insertion.
	insertionCenter := insertion size // 2 max: 1.
	editor selectFrom: editor startIndex + insertionCenter to: editor startIndex + (insertionCenter - 1).
	self invalidateEditorMorph.
	^ true