Extension { #name : #UITheme }

{ #category : #'*Morphic-Widgets-FastTable' }
UITheme >> fastTableColumnResizingColor [
	^ self selectionColor
]

{ #category : #'*Morphic-Widgets-FastTable' }
UITheme >> fastTableColumnSplitterSelectedColor [
	^ self selectionColor alpha: 0.3
]

{ #category : #'*Morphic-Widgets-FastTable' }
UITheme >> fastTableHeaderColor [
	^ self lightBackgroundColor
]
