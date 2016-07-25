*Text-Core
endOfParagraphBefore: aNumber
	"Return the index of the last Character cr before position aNumber, or zero if this is the first paragraph.
	'ddd' endOfParagraphBefore: 3
	'dd
	d' endOfParagraphBefore: 4
	"
	^ self lastIndexOf: Character cr startingAt: aNumber - 1 ifAbsent: [ 0 ]