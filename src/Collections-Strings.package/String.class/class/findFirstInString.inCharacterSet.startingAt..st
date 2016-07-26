primitives
findFirstInString: aString inCharacterSet: aCharacterSet startingAt: start 
	"Trivial, non-primitive version"
	
	start
		to: aString size
		do: [:i | (aCharacterSet
					includes: (aString at: i))
				ifTrue: [^ i]].
	^ 0