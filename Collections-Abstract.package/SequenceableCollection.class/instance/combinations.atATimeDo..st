enumerating
combinations: kk atATimeDo: aBlock
	"Take the items in the receiver, kk at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size take: kk) combinations."
	" 'abcde' combinations: 3 atATimeDo: [:each | Transcript cr; show: each printString]"

	| aCollection |
	aCollection := Array new: kk.
	self combinationsAt: 1 in: aCollection after: 0 do: aBlock