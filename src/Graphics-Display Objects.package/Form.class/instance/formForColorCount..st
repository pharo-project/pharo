other
formForColorCount: colorCount
	"Return a ColorForm of sufficient depth to represent the given number of colors. The maximum number of colors is 256."

	colorCount > 256 ifTrue: [^ self error: 'too many colors'].

	colorCount > 16 ifTrue: [^ ColorForm extent: self extent depth: 8].
	colorCount > 4 ifTrue: [^ ColorForm extent: self extent depth: 4].
	colorCount > 2 ifTrue: [^ ColorForm extent: self extent depth: 2].
	^ ColorForm extent: self extent depth: 1
