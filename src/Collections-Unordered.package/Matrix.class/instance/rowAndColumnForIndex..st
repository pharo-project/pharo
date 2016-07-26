private
rowAndColumnForIndex: index
	|t|

	t := index - 1.
	^(t // numberOfColumns + 1)@(t \\ numberOfColumns + 1)