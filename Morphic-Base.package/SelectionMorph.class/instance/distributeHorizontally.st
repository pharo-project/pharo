halo commands
distributeHorizontally
	"Distribute the empty vertical space in a democratic way."
	| minLeft maxRight totalWidth currentLeft space |

	self selectedItems size > 2
		ifFalse: [^ self].

	minLeft := self selectedItems anyOne left.
	maxRight := self selectedItems anyOne right.
	totalWidth := 0.
	self selectedItems
		do: [:each | 
			minLeft := minLeft min: each left.
			maxRight := maxRight max: each right.
			totalWidth := totalWidth + each width].

	currentLeft := minLeft.
	space := (maxRight - minLeft - totalWidth / (self selectedItems size - 1)) rounded.
	(self selectedItems
		asSortedCollection: [:x :y | x left <= y left])
		do: [:each | 
			each left: currentLeft.
			currentLeft := currentLeft + each width + space].

	self changed
