converting
withNoLineLongerThan: aNumber
	"Answer a string with the same content as receiver, but rewrapped so that no line has more characters than the given number"
	(aNumber isNumber not or: [ aNumber < 1 ]) ifTrue: [self error: 'too narrow'].
	^self class
		new: self size * (aNumber + 1) // aNumber "provision for supplementary line breaks"
		streamContents: [ :stream |
			self lineIndicesDo: [ :start :endWithoutDelimiters :end |
				| pastEnd lineStart |
				pastEnd := endWithoutDelimiters + 1.
				"eliminate spaces at beginning of line"
				lineStart := (self indexOfAnyOf: CSNonSeparators startingAt: start ifAbsent: [pastEnd]) min: pastEnd.
				[| lineStop lineEnd spacePosition |
				lineEnd := lineStop  := lineStart + aNumber min: pastEnd.
				spacePosition := lineStart.
				[spacePosition < lineStop] whileTrue: [
					spacePosition := self indexOfAnyOf: CSSeparators startingAt: spacePosition + 1 ifAbsent: [pastEnd].
					spacePosition <= lineStop ifTrue: [lineEnd := spacePosition].
				].
				"split before space or before lineStop if no space"
				stream nextPutAll: (self copyFrom: lineStart to: lineEnd - 1).
				"eliminate spaces at beginning of next line"
				lineStart := self indexOfAnyOf: CSNonSeparators startingAt: lineEnd ifAbsent: [pastEnd].
				lineStart <= endWithoutDelimiters ]
					whileTrue: [stream cr].
				stream nextPutAll: (self copyFrom: pastEnd to: end) ] ]