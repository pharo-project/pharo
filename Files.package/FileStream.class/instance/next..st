accessing
next: anInteger

	| newCollection howManyRead increment |
	newCollection := self collectionSpecies new: anInteger.
	howManyRead := 0.
	[howManyRead < anInteger] whileTrue:
		[self atEnd ifTrue:
			[(howManyRead + 1) to: anInteger do: [:i | newCollection at: i put: (self next)].
			^newCollection].
		increment := (readLimit - position) min: (anInteger - howManyRead).
		newCollection replaceFrom: (howManyRead + 1)
			to: (howManyRead := howManyRead + increment)
			with: collection
			startingAt: (position + 1).
		position := position + increment].
	^newCollection