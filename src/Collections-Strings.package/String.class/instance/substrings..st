converting
substrings: separators
	"Answer an array of non-empty substrings from the receiver separated by
	one or more characters from the 'separators' argument collection."

	| substrings substringStart |
	substrings := (Array new: 10) writeStream.
	1 to: self size do: [ :i | 
		| nextChar |
		nextChar := self at: i.
		(separators includes: nextChar)
			ifTrue: [
				substringStart
					ifNotNil: [
						substrings nextPut: (self copyFrom: substringStart to: i - 1).
						substringStart := nil ] ]
			ifFalse: [ substringStart ifNil: [ substringStart := i ] ] ].
	substringStart
		ifNotNil: [ substrings nextPut: (self copyFrom: substringStart to: self size) ].
	^ substrings contents