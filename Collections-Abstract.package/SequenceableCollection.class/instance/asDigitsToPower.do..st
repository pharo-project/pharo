enumerating
asDigitsToPower: anInteger do: aBlock
	"Repeatedly value aBlock with a single Array.  Adjust the collection
	so that aBlock is presented all (self size raisedTo: anInteger) possible 
	combinations of the receiver's elements taken as digits of an anInteger long number."
	"(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript cr; show: each printString]"

	| aCollection |
	aCollection := Array new: anInteger.
	self asDigitsAt: 1 in: aCollection do: aBlock