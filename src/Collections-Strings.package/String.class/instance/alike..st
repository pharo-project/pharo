comparing
alike: aString 
	"Answer some indication of how alike the receiver is to the argument,  0 is no match, twice aString size is best score.  Case is ignored."

	| i j k minSize bonus |
	minSize := (j := self size) min: (k := aString size).
	bonus := (j - k) abs < 2 ifTrue: [ 1 ] ifFalse: [ 0 ].
	i := 1.
	[(i <= minSize) and: [((super at: i) bitAnd: 16rDF)  = ((aString at: i) asciiValue bitAnd: 16rDF)]]
		whileTrue: [ i := i + 1 ].
	[(j > 0) and: [(k > 0) and:
		[((super at: j) bitAnd: 16rDF) = ((aString at: k) asciiValue bitAnd: 16rDF)]]]
			whileTrue: [ j := j - 1.  k := k - 1. ].
	^ i - 1 + self size - j + bonus. 