editing keys
changeEmphasis: aKeyboardEvent
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis. Emphasis change amounts to a font change.  Keeps typeahead."

	"control 0..9 -> 0..9"

	| keyCode attribute oldAttributes index thisSel colors |
	keyCode := ('0123456789-=' indexOf: aKeyboardEvent keyCharacter ifAbsent: [1]) - 1.
	oldAttributes := self text attributesAt: self pointIndex.
	thisSel := self selection.

	"Decipher keyCodes for Command 0-9..."
	"
	(keyCode between: 1 and: 5) 
		ifTrue: [attribute := TextFontChange fontNumber: keyCode].
	"
	keyCode = 6 
		ifTrue: [
			colors := #(#black #magenta #red #yellow #green #blue #cyan #white).
			index := UIManager default chooseFrom:  colors title: 'choose color...'.
			index = 0 ifTrue: [^true].
			index <= colors size 
				ifTrue: [attribute := TextColor color: (Color perform: (colors at: index))]
				ifFalse: [
					index := index - colors size - 1.	"Re-number!!!"
					index = 0 ifTrue: [attribute := self chooseColor].
					thisSel ifNil: [^true]	"Could not figure out what to link to"]].
	(keyCode between: 7 and: 11) 
		ifTrue: [
			aKeyboardEvent shiftPressed 
				ifTrue: [
					keyCode = 10 ifTrue: [attribute := TextKern kern: -1].
					keyCode = 11 ifTrue: [attribute := TextKern kern: 1]]
				ifFalse: [
					attribute := TextEmphasis 
								perform: (#(#bold #italic #narrow #underlined #struckOut) at: keyCode - 6).
					oldAttributes 
						do: [:att | (att dominates: attribute) ifTrue: [attribute turnOff]]]].
	keyCode = 0 ifTrue: [ attribute := TextEmphasis normal ].
	attribute ifNotNil: [
		self applyAttribute: attribute].
	^true