halo commands
distributeVertically
	"Distribute the empty vertical space in a democratic way."
	| minTop maxBottom totalHeight currentTop space |
	self selectedItems size > 2
		ifFalse: [^ self].

	minTop := self selectedItems anyOne top.
	maxBottom := self selectedItems anyOne bottom.
	totalHeight := 0.
	self selectedItems
		do: [:each | 
			minTop := minTop min: each top.
			maxBottom := maxBottom max: each bottom.
			totalHeight := totalHeight + each height].

	currentTop := minTop.
	space := (maxBottom - minTop - totalHeight / (self selectedItems size - 1)) rounded.
	(self selectedItems asSortedCollection:[:x :y | x top <= y top])
		do: [:each | 
			each top: currentTop.
			currentTop := currentTop + each height + space].

	self changed
