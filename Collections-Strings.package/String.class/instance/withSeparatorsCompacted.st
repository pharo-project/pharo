converting
withSeparatorsCompacted
	"Return a copy of the receiver with each sequence of whitespace (separator) 
	characters replaced by a single space character"

	self isEmpty ifTrue: [ ^ self ].
	^ self class new: self size streamContents: [:stream | 
		| lastBlank |
		lastBlank := false.
		self do: [ :eachChar | 
			lastBlank 
				ifTrue: [
					(lastBlank := eachChar isSeparator)
						ifFalse: [ stream nextPut: eachChar ] ] 
				ifFalse: [
					(lastBlank := eachChar isSeparator)
						ifTrue: [ stream nextPut: $  ]
						ifFalse: [ stream nextPut: eachChar ] ] ] ]

	"' test ' withSeparatorsCompacted"
	"' test test' withSeparatorsCompacted"
	"'test  test		 ' withSeparatorsCompacted"