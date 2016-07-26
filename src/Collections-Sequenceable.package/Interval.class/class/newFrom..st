instance creation
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

    | newInterval n |

    (n := aCollection size) <= 1 ifTrue: [
		n = 0 ifTrue: [^self from: 1 to: 0].
		^self from: aCollection first to: aCollection last].
    	newInterval := self from: aCollection first to: aCollection last
	by: (aCollection last - aCollection first) // (n - 1).
	aCollection ~= newInterval
		ifTrue: [
			"Give a second chance, because progression might be arithmetic, but = answer false"
			(newInterval hasEqualElements: aCollection) ifFalse: [
				self error: 'The argument is not an arithmetic progression']].
	^newInterval

"	Interval newFrom: {1. 2. 3}
	{33. 5. -23} as: Interval
	{33. 5. -22} as: Interval    (an error)
	(-4 to: -12 by: -1) as: Interval
	#(2 4 6) asByteArray as: Interval.
"