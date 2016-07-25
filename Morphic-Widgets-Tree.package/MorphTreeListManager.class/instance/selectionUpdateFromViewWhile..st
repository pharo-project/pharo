selection change
selectionUpdateFromViewWhile: aBlock
	| prev |
	prev := self isSelectionUpdateFromView.
	isSelectionUpdateFromView := true.
	aBlock ensure: [isSelectionUpdateFromView := prev]
