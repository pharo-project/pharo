tree
groups
	^ self groupManager groups groups select: [ :group | group isFillable and: [ group isReadOnly not ] ]