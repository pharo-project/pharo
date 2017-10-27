*Morphic-Base-keymapping
on: aShortcut do: anAction
	
	^ self bindKeyCombination: aShortcut toAction: anAction 