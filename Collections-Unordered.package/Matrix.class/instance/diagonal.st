row/column operations
diagonal
	"Answer (1 to: (numberOfRows min: numberOfColumns)) collect: [:i | self at: i at: i]"
	|i|
	i := numberOfColumns negated.
	^ (1 to: (numberOfRows min: numberOfColumns)) collect: [:j | contents at: (i := i + numberOfColumns + 1)]