keyboard
smartBackspace
	| opposite currentText currentEditor smartCharacter |
	
	currentEditor := editor.
	currentEditor hasSelection
		ifTrue: [ ^ false ].
	
	currentText := currentEditor text.
	smartCharacter := currentText at: currentEditor startIndex - 1 ifAbsent: [ ^ false ].	"take the opposite"
	
	opposite := self smartCharactersMapping at: smartCharacter ifAbsent: [ ^ false ].	"test if the next char is opposite"
	
	opposite = (currentText at: currentEditor stopIndex ifAbsent: [ ^ false ])
		ifFalse: [ ^ false ].	"test if there is an extra opposite to remove"
	
	(self 
		smartNeedExtraRemoveIn: currentText 
		for: smartCharacter 
		opposite: opposite 
		at: currentEditor startIndex)
			ifFalse: [  ^ false ].	
			
	currentEditor closeTypeIn.
	
	currentEditor 
		selectInvisiblyFrom: currentEditor startIndex - 1 
		to: currentEditor stopIndex.
	currentEditor replaceSelectionWith: currentEditor nullText.
	
	self invalidateEditorMorph.
	
	^ true