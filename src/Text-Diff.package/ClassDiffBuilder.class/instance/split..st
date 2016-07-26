private
split: aString
	"I return an array with aString splitted by Character >> #separators."

	^Array streamContents: [ :stream |
		| out |
		out := aString copy writeStream.
		aString do: [ :c |
			out nextPut: c.
			c isSeparator ifTrue:[
				stream nextPut: out contents.
				out reset ] ].
		out position = 0 ifFalse: [ 
			stream nextPut: out contents ] ]