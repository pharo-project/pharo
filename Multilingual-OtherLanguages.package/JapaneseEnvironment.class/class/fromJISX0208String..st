language methods
fromJISX0208String: aString

	^ aString collect: [:each | Character leadingChar: JapaneseEnvironment leadingChar code: (each asUnicode)].
