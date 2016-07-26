private
classify: aCharacter 
	| index |
	aCharacter isNil ifTrue: [^nil].
	index := aCharacter asInteger.
	index == 0 ifTrue: [^#separator].
	index > 255 
		ifTrue: 
			[^aCharacter isLetter 
				ifTrue: [#alphabetic]
				ifFalse: [aCharacter isSeparator ifTrue: [#separator] ifFalse: [nil]]].
	^classificationTable at: index