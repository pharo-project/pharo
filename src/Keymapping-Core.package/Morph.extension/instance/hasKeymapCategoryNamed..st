*Keymapping-Core
hasKeymapCategoryNamed: aString

	^ self kmDispatcher targets anySatisfy: [ :e | e category name = aString ].