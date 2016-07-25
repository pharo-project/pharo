public
skipSeparators

	[self atEnd not and: [ self peek isSeparator ]]
		whileTrue: [ decoratedStream next ]