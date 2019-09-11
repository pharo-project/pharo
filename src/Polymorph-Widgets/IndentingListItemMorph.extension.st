Extension { #name : #IndentingListItemMorph }

{ #category : #'*Polymorph-Widgets' }
IndentingListItemMorph >> changed [
	"Need to invalidate the selection frame."

	container ifNil: [^super changed].
	self invalidRect: self selectionFrame.
	super changed
]

{ #category : #'*Polymorph-Widgets' }
IndentingListItemMorph >> drawMouseDownHighlightOn: aCanvas [
	"Draw with a dotted border."
	
	|frame|
	self highlightedForMouseDown ifTrue: [
		container ifNil: [^super drawMouseDownHighlightOn: aCanvas].
		frame := self selectionFrame.
		aCanvas 
			frameRectangle: frame
			width: 1
			colors: {container mouseDownHighlightColor. Color transparent}
			 dashes: #(1 1)]
]

{ #category : #'*Polymorph-Widgets' }
IndentingListItemMorph >> lastChild [
	"Answer the last child."

	|c|
	c := self firstChild ifNil: [^nil].
	[c nextSibling isNil] whileFalse: [c := c nextSibling].
	^c
]

{ #category : #'*Polymorph-Widgets' }
IndentingListItemMorph >> measureContents [
	"Round up in case fractional."

	| f iconWidth |
	f := self fontToUse.
	iconWidth := self hasIcon
				ifTrue: [self icon width + 2]
				ifFalse: [0].
	^ ((13 * indentLevel + 15 + iconWidth + (contents widthToDisplayInTree: self) max: self minimumWidth)
		@ ((contents heightToDisplayInTree: self) max: f height) + (self layoutInset * 2)) ceiling
]

{ #category : #'*Polymorph-Widgets' }
IndentingListItemMorph >> openItemPath: anArray [
	"Open a path based on wrapper item equivalence. Generally more specific
	than #openPath: (string based)."
	
	| found |
	anArray isEmpty
		ifTrue: [^ container setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents withoutListWrapper == anArray first
							or: [anArray first isNil])
						ifTrue: [found := each]]].
	found
		ifNotNil: [found isExpanded
				ifFalse: [found toggleExpandedState.
					container adjustSubmorphPositions].
			found changed.
			anArray size = 1
				ifTrue: [^ container setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild openItemPath: anArray allButFirst]].
	^container setSelectedMorph: nil
]

{ #category : #'*Polymorph-Widgets' }
IndentingListItemMorph >> outerBounds [
	"Return the 'outer' bounds of the receiver, e.g., the bounds that need to be invalidated when the receiver changes."
	
	|box|
	box := super outerBounds.
	container ifNil: [^box].
	^box left: (box left min: self selectionFrame left)
]

{ #category : #'*Polymorph-Widgets' }
IndentingListItemMorph >> selectionFrame [
	"Answer the selection frame rectangle."

	|frame|
	frame := self bounds: self bounds in: container.
	frame := self
		bounds: ((frame left: container innerBounds left) right: container innerBounds right)
		from: container.
	^frame
]
