enumerating
do: elementBlock separatedBy: separatorBlock
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	1 to: self size do:
		[:index |
		index = 1 ifFalse: [separatorBlock value].
		elementBlock value: (self at: index)]