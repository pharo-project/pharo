truncation and round off
truncateTo: grid
	"Answer a Rectangle whose origin and corner are truncated to grid x and grid y."

	^Rectangle origin: (origin truncateTo: grid)
				corner: (corner truncateTo: grid)