arithmetic
alignedTo: anInteger
	"Answer the smallest number not less than receiver that is a multiple of anInteger."

	^(self+anInteger-1//anInteger)*anInteger

"5 alignedTo: 2"
"12 alignedTo: 3"