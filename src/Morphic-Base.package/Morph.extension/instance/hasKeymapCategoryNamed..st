*Morphic-Base-keymapping
hasKeymapCategoryNamed: aString

	^ self kmDispatcher targets anySatisfy: [ :e | e category name = aString ].