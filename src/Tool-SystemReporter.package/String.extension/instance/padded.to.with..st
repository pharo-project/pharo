*Tool-SystemReporter
padded: leftOrRight to: length with: aCharacter

	| result paddingLength |
	(paddingLength := length - self size) <= 0 ifTrue: [ ^ self ].
	result := self species new: length.
	leftOrRight = #left ifTrue: [
		^ result
			from: 1
				to: paddingLength
				put: aCharacter;
			replaceFrom: paddingLength + 1
				to: length
				with: self
				startingAt: 1;
			yourself ].
	leftOrRight = #right ifTrue: [
		^ result
			replaceFrom: 1
				to: self size
				with: self
				startingAt: 1;
			from: self size + 1
				to: length
				put: aCharacter;
			yourself ]