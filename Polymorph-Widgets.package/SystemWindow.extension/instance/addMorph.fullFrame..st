*Polymorph-Widgets
addMorph: aMorph fullFrame: aLayoutFrame
	"Add a morph to the receiver with the given layout frame."
	
	| left right bottom top windowBorderWidth frame |
	frame := aLayoutFrame asLayoutFrame.
	windowBorderWidth := self class borderWidth.

	left := frame leftOffset.
	right := frame rightOffset.

	bottom := frame bottomOffset.
	top := frame topOffset.
	
	frame rightFraction = 1 ifTrue: [frame rightOffset: right - windowBorderWidth - self theme windowRightOffset].
	frame leftFraction = 0
		ifTrue: [frame leftOffset: left + windowBorderWidth + self theme windowLeftOffset]
		ifFalse: [frame leftFraction = 1 ifFalse: [
					frame leftOffset: left + ProportionalSplitterMorph splitterWidth]].

	frame bottomFraction = 1 ifTrue: [frame bottomOffset: bottom - windowBorderWidth - self theme windowBottomOffset].
	frame topFraction = 0
		ifTrue: [frame topOffset: top+ self theme windowTopOffset]
		ifFalse: [frame topFraction = 1 ifFalse: [
					frame topOffset: top + ProportionalSplitterMorph splitterWidth]].
	
	super addMorph: aMorph fullFrame: frame.

	paneMorphs := paneMorphs copyReplaceFrom: 1 to: 0 with: (Array with: aMorph).
	"aMorph adoptPaneColor: self paneColor."
	aMorph
		borderStyle: (self theme windowPaneBorderStyleFor: aMorph in: self)";
		color: (aMorph initialColorInSystemWindow: self)".
	self addMorphBack: aMorph. "reorder panes so flop-out right-side scrollbar is visible"
		
	self owner ifNotNil: [self addPaneSplitters] "do when opened for performance"