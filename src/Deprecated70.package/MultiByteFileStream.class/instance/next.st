accessing
next

	| char secondChar state |
	char := (converter ifNil: [ self converter ]) nextFromStream: self.
	(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
		 ifTrue: [
			char == Cr ifTrue: [
				state := converter saveStateOf: self.
				secondChar := self bareNext.
				secondChar ifNotNil: [
					secondChar == Lf ifFalse: [ converter restoreStateOf: self with: state ] ].
				^Cr ].
			char == Lf ifTrue: [
				^Cr ] ].
	^char.

