layout
computeMorphWidths
	| paneWidths widths |
	paneWidths := self paneWidthsToFit: self totalPaneWidth.
	widths := OrderedCollection new.
	paneWidths do: [:w | widths add: w] separatedBy: [widths add: self separatorWidth].
	^ widths asArray
