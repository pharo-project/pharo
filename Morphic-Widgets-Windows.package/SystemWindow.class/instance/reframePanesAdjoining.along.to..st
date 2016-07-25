resize/collapse
reframePanesAdjoining: growingPane along: side to: aDisplayBox 
	| delta newRect minDim theMin horiz |
	growingPane ifNil: [^ self].  "As from click outside"
	newRect := aDisplayBox.
	horiz := #(left right) includes: side.
	theMin := horiz ifTrue: [40] ifFalse: [20].

	"First check that this won't make any pane smaller than theMin screen dots"
	minDim := (((paneMorphs select: [:pane | pane bounds bordersOn: growingPane bounds along: side])
		collect: [:pane | pane bounds adjustTo: newRect along: side]) copyWith: aDisplayBox)
			inject: 999 into:
				[:was :rect | was min: (horiz ifTrue: [rect width] ifFalse: [rect height])].
	"If so, amend newRect as required"
	minDim > theMin ifFalse:
		[delta := minDim - theMin.
		newRect := newRect withSide: side setTo: 
				((newRect perform: side) > (growingPane bounds perform: side)
					ifTrue: [(newRect perform: side) + delta]
					ifFalse: [(newRect perform: side) - delta])].

	"Now adjust all adjoining panes for real"
	paneMorphs do:
		[:pane | (pane bounds bordersOn: growingPane bounds along: side) ifTrue:
			[pane bounds: (pane bounds adjustTo: newRect along: side)]].
	"And adjust the growing pane itself"
	growingPane bounds: newRect.

	"Finally force a recomposition of the whole window"
	self setPaneRectsFromBounds.
	self extent: self extent