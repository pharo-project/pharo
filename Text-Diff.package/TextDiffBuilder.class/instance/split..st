private
split: aString
	"I return an Array of strings which are the lines extracted from aString. All lines contain the line separator characters, or not depending on preference."

	^Array streamContents: [ :stream |
		self class ignoreLineEndings
			ifTrue: [aString lineIndicesDo: [ :start :endWithoutSeparators :end |
				stream nextPut: (aString copyFrom: start to: endWithoutSeparators) ] ]
			ifFalse: [aString lineIndicesDo: [ :start :endWithoutSeparators :end |
				stream nextPut: (aString copyFrom: start to: end) ] ] ]