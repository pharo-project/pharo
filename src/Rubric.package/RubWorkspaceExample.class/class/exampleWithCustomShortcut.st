examples
exampleWithCustomShortcut

	 |  window text |
    text := RubScrolledTextMorph new.

    "define a custom shortcut  - notice  the call to textArea"

    text textArea removeKeyCombination: $s meta.
	 text setText: 'Hit cmd $s to revert the text'.
    text textArea on: $s meta do:[
        text setText: text text asString reverse].

    window := StandardWindow new.
    window addMorph: text fullFrame: (0@0 corner: 1@1) asLayoutFrame.
    window title: 'Custom shortcuts example'.
    window openInWorld.