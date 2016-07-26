private
findMatches
	"I find the matching pairs of xLines and yLines. First I filter out all lines that can't have a pair, then I find the longest common subsequence of the remaining elements. Finally I mark the matching pairs."

	| temp lcs xFilteredLines yFilteredLines xNumbers yNumbers |
	"Filter out all lines that can't have a pair."
	temp := yLines asSet.
	xFilteredLines := xLines select: [ :each | temp includes: each ].
	xFilteredLines size = 0
		ifTrue: [ ^ self ].
	temp := xLines asSet.
	yFilteredLines := yLines select: [ :each | temp includes: each ].
	yFilteredLines size = 0
		ifTrue: [ ^ self ].	"Map all lines to SmallIntegers, because they can be compared faster."
	temp := Dictionary new.
	xNumbers := xFilteredLines collect: [ :each | temp at: each ifAbsentPut: [ temp size ] ].
	yNumbers := yFilteredLines collect: [ :each | temp at: each ifAbsentPut: [ temp size ] ].
	temp := nil.	"Find the longest common subsequence."
	lcs := self lcsFor: xNumbers and: yNumbers.	"Mark the matching pairs."
	[ lcs isNil ]
		whileFalse: [ 
			(xFilteredLines at: (lcs at: 1)) matches: (yFilteredLines at: (lcs at: 2)).
			lcs := lcs at: 3 ]